{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.App
    ( TodoKey
    , TodosCommand'
    , TodosAction'
    , TodosModel'
    , Callbacks(..)
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
    ) where

import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo
import qualified Data.JSString as J

type TodoKey = Int

type TodosCommand' = (TodoKey, TD.Todo.Command)

type TodosAction' = (TodoKey, TD.Todo.Action)

type TodosModel' = M.Map TodoKey TD.Todo.Model

type RenderSeqNum = Int

data Command
    -- General Application level commands
    -- | This should result in React component @setState({ seqNum: RenderSeqNum })@
    = RenderRequiredCommand -- FIXME: SeqNum
    -- | This should run dispose on the SomeDisposable (eg. to release Callbacks)
    | DisposeCommand CD.SomeDisposable
    -- | This should result in passing a callback factory into the argument function which produces a cmd,
    -- | Then evaluating that resulting cmd.
    | MakeCallbacksCommand (((J.JSVal -> MaybeT IO Action) -> IO (J.Callback (J.JSVal -> IO ()))) -> IO Command)
    -- | Send the action to the application output address.
    | SendActionCommand Action

    -- TODO specific commands
    | InputCommand TD.Input.Command
    | TodosCommand TodosCommand'


data Action
    -- General Application level actions
    = RenderUpdatedAction RenderSeqNum
    | DeferCommandAction RenderSeqNum Command
    -- TODO specific actions
    | ToggleCompleteAllAction
    | DestroyTodoAction TodoKey
    | RequestNewTodoAction J.JSString
    | AddNewTodoAction Int TD.Todo.Model
    | InputAction TD.Input.Action
    | TodosAction TodosAction'

makeClassyPrisms ''Action

data Callbacks = Callbacks
    { fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

instance CD.Disposing Callbacks

data Model = Model
    { uid :: J.JSString
    , renderSeqNum :: RenderSeqNum
    , deferredCommands :: M.Map RenderSeqNum (D.DList Command)
    , todoSeqNum :: Int
    , todoInput :: TD.Input.Model
    , todosModel :: TodosModel'
    , callbacks :: Callbacks
    }

makeClassy_ ''Model

hasActiveTodos :: TodosModel' -> Bool
hasActiveTodos = not . null . filter (not . TD.Todo.completed) . fmap snd . M.toList

toggleCompleteAllFirer :: Applicative m => J.JSVal -> m Action
toggleCompleteAllFirer = const $ pure ToggleCompleteAllAction

mapInputHandler :: (J.JSVal -> MaybeT IO TD.Input.Action) -> J.JSVal -> MaybeT IO Action
mapInputHandler f v = (review _InputAction) <$> f v

mapTodoHandler :: Functor m => TodoKey -> (J.JSVal -> m TD.Todo.Action) -> J.JSVal -> m Action
mapTodoHandler k f v = (\a -> TodosAction (k, a)) <$> f v

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.bh "header" [ ("key", J.jsval $ uid s)
                         , ("className", E.strval "header")
                         ] $ do
        R.bh "h1" [("key", E.strval "heading")] (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT mainWindow s

mainWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
mainWindow = do
    todos <- view _todosModel
    if (null todos)
        then pure ()
        else do
        s <- ask
        lift $ R.bh "section" [ ("key", E.strval "main")
                              , ("className", E.strval "main")
                              ] $ do
            -- This is the complete all checkbox
            R.lf (E.strval "input")
                        [ ("key", E.strval "toggle-all")
                        , ("className", E.strval "toggle-all")
                        , ("type", E.strval "checkbox")
                        , ("checked", J.pToJSVal . not . hasActiveTodos $ todos)
                        , ("onChange", J.jsval $ fireToggleCompleteAll $ callbacks s)
                        ]
            view G._WindowT todoListWindow s

todoListWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
todoListWindow = do
    todos <- fmap snd . M.toList <$> view _todosModel
    lift $ R.bh "ul" [ ("key", E.strval "todo-list")
                     , ("className", E.strval "todo-list")
                     ] $
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

        DeferCommandAction i cmd -> do
            _deferredCommands %= (M.alter (addCommand cmd) i)
            pure $ D.singleton RenderRequiredCommand

        DestroyTodoAction k -> do
            -- queue up callbacks to be released after rerendering
            ts <- use _todosModel
            ret <- runMaybeT $ do
                todoModel <- MaybeT $ pure $ M.lookup k ts
                let junk = CD.disposing (TD.Todo.callbacks todoModel)
                i <- use _renderSeqNum
                _deferredCommands %= (M.alter (addCommand $ DisposeCommand junk) i)
                -- Remove the todo from the model
                _todosModel %= M.delete k
                pure $ D.singleton RenderRequiredCommand
            maybe (pure mempty) pure ret

        RenderUpdatedAction n -> do
            -- All deferred commands with renderSeqNum lower than the rendered SeqNum is
            -- safe to run
            -- Similarly run delayed commands that need to wait until a particular frame is rendered
            -- Eg focusing after other rendering changes
            cmds <- use _deferredCommands
            let (cmds', leftoverCmds) = M.partitionWithKey (\k _ -> k < n) cmds
            _deferredCommands .= leftoverCmds

            pure . foldMap snd . M.toList $ cmds'

        RequestNewTodoAction str -> do
            n <- use _todoSeqNum
            _todoSeqNum %= (+ 1)
            pure $ D.singleton $ MakeCallbacksCommand $ \f -> do
                cbs <- TD.Todo.mkCallbacks $ f . mapTodoHandler n
                pure $ SendActionCommand $ AddNewTodoAction n $ TD.Todo.Model (J.pack . show $ n)
                    str
                    False
                    J.empty
                    J.nullRef
                    cbs

        AddNewTodoAction n s -> do
            _todosModel %= M.insert n s
            pure $ D.singleton RenderRequiredCommand

        -- these will be handled by monoidally appending other gadgets
        InputAction _ -> pure mempty
        TodosAction _ -> pure mempty
  where
    addCommand cmd Nothing = Just $ D.singleton cmd
    addCommand cmd (Just cmds') = Just (cmds' `D.snoc` cmd)

    addDisposable junk Nothing = Just junk
    addDisposable junk (Just junk') = Just (junk' <> junk)

    toggleCompleteAll :: M.Map TodoKey TD.Todo.Model -> M.Map TodoKey TD.Todo.Model
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
    case M.lookup k s of
        Nothing -> pure mempty
        Just s' -> do
            -- run the todo gadget logic
            (cmds, s'') <- lift $ view G._GadgetT TD.Todo.gadget a s'
            put $ M.insert k s'' s
            -- annotate cmd with the key
            pure $ (\cmd -> (k, cmd)) <$> cmds

todosGadget' :: Monad m => G.GadgetT Action Model m (D.DList Command)
todosGadget' = fmap TodosCommand <$> zoom _todosModel (magnify _TodosAction todosGadget)
