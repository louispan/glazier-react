{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.App.Run
    ( interpretCommand
    , mkActionCallback
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Pipes.Concurrent as PC
import qualified Todo.App as TD.App
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo
import qualified Glazier.React.Maker.Run as R

-- FIXME: Move to Maker.Run.hs?
mkActionCallback
    :: PC.Output TD.App.Action
    -> (J.JSVal -> MaybeT IO TD.App.Action)
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            action <- handler evt
            lift $ void $ atomically $ PC.send output action

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
interpretCommand
    :: (MonadIO io, MonadState TD.App.MModel io)
    => PC.Output TD.App.Action
    -> TD.App.Command
    -> io ()

interpretCommand output (TD.App.MakerCommand mks) = do
    act <- liftIO $ iterM (R.runMaker output) mks
    liftIO $ void $ atomically $ PC.send output act

interpretCommand _      TD.App.RenderCommand = do
    -- increment the sequence number if render is required
    TD.App.model . TD.App.frameNum  %= (+ 1)
    TD.App.MModel (ms, s) <- get
    liftIO . void $ swapMVar ms s -- ^ so that the render callback can use the latest state
    let i = s ^. TD.App.model . TD.App.frameNum
    ref <- use (TD.App.model . TD.App.ref)
    liftIO $ js_setStateFrameNum ref i -- ^ notify React that the specific component has changed

interpretCommand _                  (TD.App.DisposeCommand x) =
    liftIO $ CD.dispose x

interpretCommand _      (TD.App.InputCommand (TD.Input.RenderCommand)) = do
    -- increment the sequence number if render is required
    TD.App.todoInput . TD.Input.model . TD.Input.frameNum  %= (+ 1)
    TD.Input.MModel (ms, s) <- use TD.App.todoInput
    liftIO . void $ swapMVar ms s -- ^ so that the render callback can use the latest state
    let i = s ^. TD.Input.model . TD.Input.frameNum
    ref <- use (TD.App.todoInput . TD.Input.model . TD.Input.ref)
    liftIO $ js_setStateFrameNum ref i -- ^ notify React that the specific component has changed

interpretCommand output             (TD.App.InputCommand (TD.Input.SubmitCommand str)) = do
    liftIO $ void $ atomically $ PC.send output (TD.App.RequestNewTodoAction str)

interpretCommand _         (TD.App.InputCommand (TD.Input.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd

interpretCommand _      (TD.App.TodosCommand (k, TD.Todo.RenderCommand)) = void $ runMaybeT $ do
    TD.Todo.MModel (ms, s) <- MaybeT $ use (TD.App.model . TD.App.todosModel . at k)
    let s' = s & TD.Todo.model . TD.Todo.frameNum %~ (+ 1)
    (TD.App.model . TD.App.todosModel . at k) .= Just (TD.Todo.MModel (ms, s'))
    liftIO . void $ swapMVar ms s' -- ^ so that the render callback can use the latest state
    let i = s' ^. TD.Todo.model . TD.Todo.frameNum
        ref = s' ^. TD.Todo.model . TD.Todo.ref
    liftIO $ js_setStateFrameNum ref i -- ^ notify React that the specific component has changed

interpretCommand output             (TD.App.TodosCommand (k, TD.Todo.DestroyCommand)) =
    liftIO $ void $ atomically $ PC.send output (TD.App.DestroyTodoAction k)

interpretCommand _                  (TD.App.TodosCommand (_, TD.Todo.FocusNodeCommand node)) =
    liftIO $ js_focus node

interpretCommand _                  (TD.App.TodosCommand (_, TD.Todo.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd
