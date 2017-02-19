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
    , window
    , gadget
    , toggleCompleteAllFirer
    , inputChangeFirer
    , inputSubmitFirer
    , producer
    ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.HashMap.Strict as M
import qualified Data.Map.Strict as Map
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
import Data.Semigroup

type TodoKey = (Int, J.JSString)

type TodosCommand' = (TodoKey, TD.Todo.Command)

type TodosAction' = (TodoKey, TD.Todo.Action)

type TodosModel' = Map.Map TodoKey TD.Todo.Model

data Command
    = StateChangedCommand
    | InputCommand TD.Input.Command
    | TodosCommand TodosCommand'

data Action
    = ToggleCompleteAllAction
    | DestroyTodoAction TodoKey
    | InputAction TD.Input.Action
    | TodosAction TodosAction'

makeClassyPrisms ''Action

data Model = Model
    { todoInput :: TD.Input.Model
    , seqNum :: Int
    , todos :: TodosModel'
    , fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    }

makeClassy_ ''Model

hasActiveTodos :: TodosModel' -> Bool
hasActiveTodos = null . filter (not . TD.Todo.completed) . fmap snd . Map.toList

toggleCompleteAllFirer :: Applicative m => J.JSVal -> m Action
toggleCompleteAllFirer = const $ pure ToggleCompleteAllAction

inputChangeFirer :: J.JSVal -> MaybeT IO Action
inputChangeFirer v = (review _InputAction) <$> TD.Input.changeFirer v

inputSubmitFirer :: J.JSVal -> MaybeT IO Action
inputSubmitFirer v = (review _InputAction) <$> TD.Input.submitFirer v

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.branch "header" (M.fromList
                       [ ("className", E.strval "header")
                       , ("key", E.strval "todo")
                       ]) $ do
        R.branch "h1" (M.singleton "key" (E.strval "heading")) (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT mainWindow s

mainWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
mainWindow = do
    todos' <- view _todos
    if (null todos')
        then pure ()
        else do
        s <- ask
        lift $ R.branch "section" (M.singleton "className" (E.strval "main")) $ do
            -- This is the complete all checkbox
            R.leaf (E.strval "input") (M.fromList
                        [ ("className", E.strval "toggle-all")
                        , ("type", E.strval "checkbox")
                        , ("checked", J.pToJSVal . not . hasActiveTodos $ todos')
                        , ("onChange", J.jsval $ fireToggleCompleteAll s)
                        ])
            view G._WindowT todoListWindow s

todoListWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
todoListWindow = do
    lift $ R.branch "ul" (M.singleton "className" (E.strval "todo-list")) $ do
        pure ()

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = appGadget
    <> inputGadget
    <> todosGadget'

appGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
appGadget = do
    a <- ask
    case a of
        ToggleCompleteAllAction -> do
            _todos %= toggleCompleteAll
            pure $ D.singleton StateChangedCommand
        DestroyTodoAction k -> do
            _todos %= Map.delete k
            pure $ D.singleton StateChangedCommand
        -- delegate to other gadgets
        InputAction _ -> pure mempty
        TodosAction _ -> pure mempty
  where
    toggleCompleteAll :: Map.Map TodoKey TD.Todo.Model -> Map.Map TodoKey TD.Todo.Model
    toggleCompleteAll xs = fmap (toggleCompleteAll' $ hasActiveTodos xs) xs
    toggleCompleteAll' :: Bool -> TD.Todo.Model -> TD.Todo.Model
    toggleCompleteAll' b s = s & TD.Todo._completed .~ b


inputWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
inputWindow = G.implant _todoInput TD.Input.window

inputGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
inputGadget = fmap InputCommand <$> G.implant _todoInput (G.dispatch _InputAction TD.Input.gadget)

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
            -- check if state need to be updated
            if (not . null . filter isStateChangedCmd . D.toList $ cmds)
               then put $ Map.insert k s'' s
               else pure ()
            -- annotate cmd with the key
            pure $ (\cmd -> (k, cmd)) <$> cmds
  where
     isStateChangedCmd cmd = case cmd of
         TD.Todo.StateChangedCommand -> True
         _ -> False

todosGadget' :: Monad m => G.GadgetT Action Model m (D.DList Command)
todosGadget' = fmap TodosCommand <$> G.implant _todos (G.dispatch _TodosAction todosGadget)

producer
    :: (MFunctor t, MonadState Model (t STM), MonadTrans t, MonadIO io)
    => PC.Input Action
    -> P.Producer' (D.DList Command) (t io) ()
producer input = hoist (hoist (liftIO . atomically)) (PM.rsProducer input (G.runGadgetT gadget))
