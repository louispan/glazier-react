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
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Pipes.Concurrent as PC
import qualified Todo.App as TD.App
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo
import qualified Todo.Dummy as TD.Dummy

mkActionCallback
    :: PC.Output TD.App.Action
    -> (J.JSVal -> MaybeT IO TD.App.Action)
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            action <- handler evt
            lift $ void $ atomically $ PC.send output action

forceRender :: (MonadIO io, MonadState TD.App.Model io) => (J.JSString -> J.JSVal -> IO ()) -> MVar TD.App.Model -> io ()
forceRender shout stateMVar = do
    -- increment the sequence number if render is required
    TD.App._renderSeqNum %= (+ 1)
    s <- get
    let i = TD.App.renderSeqNum s
    liftIO . void $ swapMVar stateMVar s -- ^ so that the render callback can use the latest state
    liftIO $ shout "forceRender" (J.pToJSVal i) -- ^ tell React to call render


foreign import javascript unsafe
  "if ($1) { $1.focus(); }"
  js_focusNode :: J.JSVal -> IO ()


foreign import javascript unsafe
  "if ($1 && $1['setSelectionRange']) { $1['setSelectionRange']($2, $3, $4); }"
  js_setSelectionRange :: J.JSVal -> Int -> Int -> J.JSString -> IO ()


foreign import javascript unsafe
  "if ($1 && $1['setState']) { $1['setState']({ seqNum : $2 }); }"
  js_setState :: J.JSVal -> Int -> IO ()


-- | Evaluate commands from gadgets here
interpretCommand
    :: (MonadIO io, MonadState TD.App.Model io)
    => (J.JSString -> J.JSVal -> IO ())
    -> MVar TD.App.Model
    -> PC.Output TD.App.Action
    -> TD.App.Command
    -> io ()

interpretCommand shout stateMVar _      TD.App.RenderRequiredCommand =
    forceRender shout stateMVar

interpretCommand _ _ _                  (TD.App.DisposeCommand x) =
    liftIO $ CD.dispose x

interpretCommand shout stateMVar output (TD.App.MakeCallbacksCommand f) = do
    cmd <- liftIO $ f (mkActionCallback output)
    interpretCommand shout stateMVar output cmd

interpretCommand _ _ output             (TD.App.SendActionCommand a) = do
    liftIO $ void $ atomically $ PC.send output a

interpretCommand shout stateMVar _      (TD.App.InputCommand (TD.Input.RenderRequiredCommand)) =
    forceRender shout stateMVar

interpretCommand _ _ output             (TD.App.InputCommand (TD.Input.SubmitCommand str)) = do
    liftIO $ void $ atomically $ PC.send output (TD.App.RequestNewTodoAction str)

interpretCommand _ _ _         (TD.App.InputCommand (TD.Input.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd

interpretCommand _ _ output             (TD.App.InputCommand (TD.Input.DeferCommand cmd)) = do
    i <- use TD.App._renderSeqNum
    liftIO . void . atomically $ PC.send output (TD.App.DeferCommandAction i (TD.App.InputCommand cmd))

interpretCommand shout stateMVar _      (TD.App.TodosCommand (_, TD.Todo.RenderRequiredCommand)) =
    forceRender shout stateMVar

interpretCommand _ _ output             (TD.App.TodosCommand (k, TD.Todo.DestroyCommand)) =
    liftIO $ void $ atomically $ PC.send output (TD.App.DestroyTodoAction k)

interpretCommand _ _ _                  (TD.App.TodosCommand (_, TD.Todo.FocusNodeCommand node)) =
    liftIO $ js_focusNode node

interpretCommand _ _ _                  (TD.App.TodosCommand (_, TD.Todo.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd

interpretCommand _ _ output             (TD.App.TodosCommand (k, TD.Todo.DeferCommand cmd)) = do
    i <- use TD.App._renderSeqNum
    let cmd' = TD.App.TodosCommand (k, cmd)
    liftIO . void . atomically $ PC.send output (TD.App.DeferCommandAction i cmd')

interpretCommand _ _ _      (TD.App.DummyCommand (TD.Dummy.RenderCommand)) = do
    -- increment the sequence number if render is required
    TD.App._todoDummy . TD.Dummy._renderSeqNum  %= (+ 1)
    s <- use TD.App._todoDummy
    modelMVar <- use TD.App._todoDummyMVar
    liftIO . void $ swapMVar modelMVar s -- ^ so that the render callback can use the latest state
    let i = TD.Dummy.renderSeqNum s
    ref <- use (TD.App._todoDummy . TD.Dummy._ref)
    liftIO $ js_setState ref i -- ^ notify React that the specific component has changed

interpretCommand _ _ output             (TD.App.DummyCommand (TD.Dummy.SubmitCommand str)) = do
    liftIO $ void $ atomically $ PC.send output (TD.App.RequestNewTodoAction str)

interpretCommand _ _ _         (TD.App.DummyCommand (TD.Dummy.SetSelectionCommand n ss se sd)) =
    liftIO $ js_setSelectionRange n ss se sd
