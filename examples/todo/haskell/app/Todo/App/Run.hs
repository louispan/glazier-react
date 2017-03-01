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
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Pipes.Concurrent as PC
import qualified Todo.App as TD.App
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo

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
interpretCommand
    :: (MonadIO io, MonadState TD.App.Model io)
    => MVar TD.App.Model
    -> PC.Output TD.App.Action
    -> TD.App.Command
    -> io ()

interpretCommand ms _      TD.App.RenderCommand = do
    -- increment the sequence number if render is required
    TD.App._frameNum  %= (+ 1)
    s <- get
    liftIO . void $ swapMVar ms s -- ^ so that the render callback can use the latest state
    let i = TD.App.frameNum s
    ref <- use (TD.App._ref)
    liftIO $ js_setStateFrameNum ref i -- ^ notify React that the specific component has changed

interpretCommand _ _                  (TD.App.DisposeCommand x) =
    liftIO $ CD.dispose x

interpretCommand stateMVar output (TD.App.MakeCallbacksCommand f) = do
    cmd <- liftIO $ f (mkActionCallback output)
    interpretCommand stateMVar output cmd

interpretCommand _ output             (TD.App.SendActionCommand a) = do
    liftIO $ void $ atomically $ PC.send output a

interpretCommand stateMVar _      (TD.App.InputCommand (TD.Input.RenderCommand)) = do
    -- increment the sequence number if render is required
    TD.App._todoInput . _2 . TD.Input._frameNum  %= (+ 1)
    (ms, s) <- use TD.App._todoInput
    liftIO . void $ swapMVar ms s -- ^ so that the render callback can use the latest state
    let i = TD.Input.frameNum s
    ref <- use (TD.App._todoInput . _2 . TD.Input._ref)
    liftIO $ js_setStateFrameNum ref i -- ^ notify React that the specific component has changed

interpretCommand _ output             (TD.App.InputCommand (TD.Input.SubmitCommand str)) = do
    liftIO $ void $ atomically $ PC.send output (TD.App.RequestNewTodoAction str)

interpretCommand _ _         (TD.App.InputCommand (TD.Input.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd

interpretCommand stateMVar _      (TD.App.TodosCommand (k, TD.Todo.RenderCommand)) = void $ runMaybeT $ do
    (ms, s) <- MaybeT $ use (TD.App._todosModel . at k)
    let s' = s & TD.Todo._frameNum %~ (+ 1)
    (TD.App._todosModel . at k) .= Just (ms, s')
    liftIO . void $ swapMVar ms s' -- ^ so that the render callback can use the latest state
    let i = TD.Todo.frameNum s'
        ref = TD.Todo.ref s'
    liftIO $ js_setStateFrameNum ref i -- ^ notify React that the specific component has changed

interpretCommand _ output             (TD.App.TodosCommand (k, TD.Todo.DestroyCommand)) =
    liftIO $ void $ atomically $ PC.send output (TD.App.DestroyTodoAction k)

interpretCommand _ _                  (TD.App.TodosCommand (_, TD.Todo.FocusNodeCommand node)) =
    liftIO $ js_focus node

interpretCommand _ _                  (TD.App.TodosCommand (_, TD.Todo.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd
