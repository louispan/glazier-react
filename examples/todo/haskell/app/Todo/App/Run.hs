{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.App.Run
    ( runCommand
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified GHCJS.Types as J
import qualified Pipes.Concurrent as PC
import qualified Todo.App as TD.App
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo
import qualified Glazier.React.Maker.Run as R
import qualified Glazier.React.Command.Run as R

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: J.JSVal -> IO ()


foreign import javascript unsafe
  "if ($1 && $1['setSelectionRange']) { $1['setSelectionRange']($2, $3, $4); }"
  js_setSelectionRange :: J.JSVal -> Int -> Int -> J.JSString -> IO ()


foreign import javascript unsafe
  "if ($1 && $1['setState']) { $1['setState']({ frameNum : $2 }); }"
  js_setStateFrameNum :: J.JSVal -> Int -> IO ()


-- | Evaluate commands from gadgets here
-- FIXME: Share render code
runCommand
    :: (MonadIO io, MonadState TD.App.SuperModel io)
    => PC.Output TD.App.Action
    -> TD.App.Command
    -> io ()

runCommand output (TD.App.MakerCommand mks) = do
    act <- liftIO $ iterM (R.runMaker output) mks
    liftIO $ void $ atomically $ PC.send output act

runCommand _      TD.App.RenderCommand = do
    sm <- use id
    R.reactSetState
        id
        TD.App.mModel
        TD.App.cModel
        (TD.App.model . TD.App.frameNum)
        (TD.App.model . TD.App.frameNum)
        (TD.App.model . TD.App.ref)
        sm

runCommand _                  (TD.App.DisposeCommand x) =
    liftIO $ CD.dispose x

runCommand _      (TD.App.InputCommand (TD.Input.RenderCommand)) = do
    sm <- use TD.App.todoInput
    R.reactSetState
        TD.App.todoInput
        TD.Input.mModel
        TD.Input.cModel
        (TD.Input.model . TD.Input.frameNum)
        (TD.Input.model . TD.Input.frameNum)
        (TD.Input.model . TD.Input.ref)
        sm

runCommand output             (TD.App.InputCommand (TD.Input.SubmitCommand str)) = do
    liftIO $ void $ atomically $ PC.send output (TD.App.RequestNewTodoAction str)

runCommand _         (TD.App.InputCommand (TD.Input.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd

runCommand _      (TD.App.TodosCommand (k, TD.Todo.RenderCommand)) = void $ runMaybeT $ do
    sm <- MaybeT $ preuse (TD.App.model . TD.App.todosModel . at k . _Just)
    R.reactSetState
        (TD.App.model . TD.App.todosModel . at k . _Just)
        TD.Todo.mModel
        TD.Todo.cModel
        (TD.Todo.model . TD.Todo.frameNum)
        (TD.Todo.model . TD.Todo.frameNum)
        (TD.Todo.model . TD.Todo.ref)
        sm

runCommand output             (TD.App.TodosCommand (k, TD.Todo.DestroyCommand)) = do
    liftIO $ void $ atomically $ PC.send output (TD.App.DestroyTodoAction k)

runCommand _                  (TD.App.TodosCommand (_, TD.Todo.FocusNodeCommand node)) =
    liftIO $ js_focus node

runCommand _                  (TD.App.TodosCommand (_, TD.Todo.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd
