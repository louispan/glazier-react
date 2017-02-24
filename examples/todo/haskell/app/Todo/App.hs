{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.App
    ( TodoKey
    , TodosCommand'
    , TodosAction'
    , TodosModel'
    , Command(..)
    , Action(..)
    , AsAction(..)
    , Model(..)
    , HasModel(..)
    , isRenderRequiredCommand
    , window
    , gadget
    , toggleCompleteAllFirer
    , mapInputHandler
    , mapTodoHandler
    , getGarbage
    , producer
    ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.Map.Strict as Map
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Misc as PM
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo
import qualified Data.JSString as J

type TodoKey = Int

type TodosCommand' = (TodoKey, TD.Todo.Command)

type TodosAction' = (TodoKey, TD.Todo.Action)

type TodosModel' = Map.Map TodoKey TD.Todo.Model

data Command
    = RenderRequiredCommand
    | TrashGarbageCommand [E.Garbage]
    | MakeCallbacksCommand (((J.JSVal -> MaybeT IO Action) -> IO (J.Callback (J.JSVal -> IO ()))) -> IO Command)
    | SendActionCommand Action
    | InputCommand TD.Input.Command
    | TodosCommand TodosCommand'

data Action
    = ToggleCompleteAllAction
    | DestroyTodoAction TodoKey
    | RenderUpdatedAction Int
    | DelayedCommands Int (D.DList Command)
    | RequestNewTodoAction J.JSString
    | AddNewTodoAction Int TD.Todo.Model
    | InputAction TD.Input.Action
    | TodosAction TodosAction'

makeClassyPrisms ''Action

data Model = Model
    { uid :: J.JSString
    , renderSeqNum :: Int
    , todoSeqNum :: Int
    , garbageDump :: Map.Map Int (D.DList E.Garbage)
    , delayedCommands :: Map.Map Int (D.DList Command)
    , todoInput :: TD.Input.Model
    , todosModel :: TodosModel'
    , fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    }

makeClassy_ ''Model

getGarbage :: Model -> [E.Garbage]
getGarbage s = [ E.scrap $ fireToggleCompleteAll s]

hasActiveTodos :: TodosModel' -> Bool
hasActiveTodos = not . null . filter (not . TD.Todo.completed) . fmap snd . Map.toList

toggleCompleteAllFirer :: Applicative m => J.JSVal -> m Action
toggleCompleteAllFirer = const $ pure ToggleCompleteAllAction

mapInputHandler :: (J.JSVal -> MaybeT IO TD.Input.Action) -> J.JSVal -> MaybeT IO Action
mapInputHandler f v = (review _InputAction) <$> f v

mapTodoHandler :: Functor m => TodoKey -> (J.JSVal -> m TD.Todo.Action) -> J.JSVal -> m Action
mapTodoHandler k f v = (\a -> TodosAction (k, a)) <$> f v

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.branch "header" (M.fromList
                       [ ("key", J.jsval $ uid s)
                       , ("className", E.strval "header")
                       ]) $ do
        R.branch "h1" (M.singleton "key" (E.strval "heading")) (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT mainWindow s

mainWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
mainWindow = do
    todos <- view _todosModel
    if (null todos)
        then pure ()
        else do
        s <- ask
        lift $ R.branch "section" (M.fromList
                        [ ("key", E.strval "main")
                        , ("className", E.strval "main")
                        ]) $ do
            -- This is the complete all checkbox
            R.leaf (E.strval "input") (M.fromList
                        [ ("key", E.strval "toggle-all")
                        , ("className", E.strval "toggle-all")
                        , ("type", E.strval "checkbox")
                        , ("checked", J.pToJSVal . not . hasActiveTodos $ todos)
                        , ("onChange", J.jsval $ fireToggleCompleteAll s)
                        ])
            view G._WindowT todoListWindow s

todoListWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
todoListWindow = do
    todos <- fmap snd . Map.toList <$> view _todosModel
    lift $ R.branch "ul"  (M.fromList
                        [ ("key", E.strval "todo-list")
                        , ("className", E.strval "todo-list")
                        ]) $ do
        traverse_ (view G._WindowT TD.Todo.window) todos

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = appGadget
    <> inputGadget
    <> todosGadget'

isRenderRequiredCommand :: Command -> Bool
isRenderRequiredCommand cmd = case cmd of
     RenderRequiredCommand -> True
     InputCommand TD.Input.RenderRequiredCommand -> True
     TodosCommand (_, TD.Todo.RenderRequiredCommand) -> True
     _ -> False

appGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
appGadget = do
    a <- ask
    case a of
        ToggleCompleteAllAction -> do
            _todosModel %= toggleCompleteAll
            pure $ D.singleton RenderRequiredCommand

        DelayedCommands i cmds -> do
            _delayedCommands %= (Map.alter (addCommands cmds) i)
            pure $ D.singleton RenderRequiredCommand

        DestroyTodoAction k -> do
            -- queue up callbacks to be released after rerendering
            ts <- use _todosModel
            ret <- runMaybeT $ do
                todoModel <- MaybeT $ pure $ Map.lookup k ts
                junk <- pure $ TD.Todo.getGarbage (TD.Todo.callbacks todoModel)
                renderSeqNum' <- use _renderSeqNum
                _garbageDump %= (Map.alter (addGarbage (D.fromList junk)) renderSeqNum')
                _todosModel %= Map.delete k
                pure $ D.singleton RenderRequiredCommand
            maybe (pure mempty) pure ret

        RenderUpdatedAction n -> do
            -- Called callbacks that have been released will generate an exception
            -- So make this into an action is evaluated after re-rendering
            -- to ensure callbacks wont get called.
            -- All garbage with lower key than seqNum are safe to be released
            dump <- use _garbageDump
            let (garbage, leftoverJunk) = Map.partitionWithKey (\k _ -> k < n) dump
            _garbageDump .= leftoverJunk

            -- Similarly run delayed commands that need to wait until a particular frame is rendered
            -- Eg focusing after other rendering changes
            cmds <- use _delayedCommands
            let (cmds', leftoverCmds) = Map.partitionWithKey (\k _ -> k < n) cmds
            _delayedCommands .= leftoverCmds

            pure $ (foldMap snd . Map.toList $ cmds') `D.snoc` TrashGarbageCommand (D.toList . foldMap snd . Map.toList $ garbage)

        RequestNewTodoAction str -> do
            n <- use _todoSeqNum
            _todoSeqNum %= (+ 1)
            pure $ D.singleton $ MakeCallbacksCommand $ \f -> do
                callbacks <- TD.Todo.mkCallbacks $ f . mapTodoHandler n
                pure $ SendActionCommand $ AddNewTodoAction n $ TD.Todo.Model (J.pack . show $ n)
                    str
                    False
                    J.empty
                    J.nullRef
                    callbacks

        AddNewTodoAction n s -> do
            _todosModel %= Map.insert n s
            pure $ D.singleton RenderRequiredCommand

        -- these will be handled by monoidally appending other gadgets
        InputAction _ -> pure mempty
        TodosAction _ -> pure mempty
  where
    addCommands cmds Nothing = Just cmds
    addCommands cmds (Just cmds') = Just (cmds' <> cmds)

    addGarbage junk Nothing = Just junk
    addGarbage junk (Just junk') = Just (junk' <> junk)

    toggleCompleteAll :: Map.Map TodoKey TD.Todo.Model -> Map.Map TodoKey TD.Todo.Model
    toggleCompleteAll xs = fmap (toggleCompleteAll' $ hasActiveTodos xs) xs
    toggleCompleteAll' :: Bool -> TD.Todo.Model -> TD.Todo.Model
    toggleCompleteAll' b s = s & TD.Todo._completed .~ b


inputWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
inputWindow = magnify _todoInput TD.Input.window

inputGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
inputGadget = fmap InputCommand <$> zoom _todoInput (magnify _InputAction TD.Input.gadget)

todosGadget :: Monad m => G.GadgetT TodosAction' TodosModel' m (D.DList TodosCommand')
todosGadget = do
    -- expect a (key, action) pair
    (k, a) <- ask
    s <- get
    case Map.lookup k s of
        Nothing -> pure mempty
        Just s' -> do
            -- run the todo gadget logic
            (cmds, s'') <- lift $ view G._GadgetT TD.Todo.gadget a s'
            put $ Map.insert k s'' s
            -- annotate cmd with the key
            pure $ (\cmd -> (k, cmd)) <$> cmds

todosGadget' :: Monad m => G.GadgetT Action Model m (D.DList Command)
todosGadget' = fmap TodosCommand <$> zoom _todosModel (magnify _TodosAction todosGadget)

producer :: PC.Input Action -> P.Producer' (D.DList Command) (StateT Model STM) ()
producer input = PM.execInput input (G.runGadgetT gadget)
