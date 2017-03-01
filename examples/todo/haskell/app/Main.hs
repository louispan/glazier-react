{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.State.Strict
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Prim as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import qualified Glazier.React.ReactDOM as RD
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Misc as PM
import qualified Pipes.Prelude as PP
import qualified Todo.App as TD.App
import qualified Todo.App.Run as TD.App.Run
import qualified Todo.Input as TD.Input

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
-- GHCJS runs 'main' lazily.
-- The code here only start running after all javascript is loaded.
-- Ie. h$main(h$mainZCZCMainzimain) just schedules the work to be executed after all javascript is loaded.
main :: IO ()
main = do
    -- Create a 'Pipes.Concurrent' mailbox for receiving actions from html events.
    -- NB. using 'PC.bounded 1' also works without deadlocks, but doesn't save memory
    -- blocked events are kept by the GHCJCS runtime.
    (output, input) <- liftIO . PC.spawn $ PC.unbounded

    -- Input Model
    inputMModel <- newEmptyMVar
    inputCallbacks <- TD.App.mkInputCallbacks inputMModel (TD.App.Run.mkActionCallback output)
    let inputModel = TD.Input.Model
            inputCallbacks
            "new-input"
            J.nullRef
            0
            mempty
            mempty

    putMVar inputMModel inputModel

    -- App Model
    appMModel <- newEmptyMVar
    appCallbacks <- TD.App.mkCallbacks appMModel (TD.App.Run.mkActionCallback output)
    let appModel = TD.App.Model
            appCallbacks
            "todos"
            J.nullRef
            0
            mempty
            0
            (inputMModel, inputModel)
            mempty -- todosModel

    putMVar appMModel appModel

    -- -- Setup the render callback
    -- -- render <- syncCallback1' (view G._WindowT' (jsval <$> counterWindow) . R.unsafeCoerceReactElement)
    -- doRender <- J.syncCallback1' $ \_ -> do
    --     s <- readMVar currentState
    --     J.pToJSVal <$> R.markedElement TD.App.window s
    -- void $ js_globalListen "renderHaskell" doRender

    -- -- Setup the callback garbage collection callback
    -- doRenderUpdated <- J.asyncCallback1 $ \seqNum -> void . runMaybeT $ do
    --     seqNum' <- MaybeT $ J.fromJSVal seqNum
    --     MaybeT . fmap guard . atomically . PC.send output $ TD.App.RenderUpdatedAction seqNum'
    -- void $ js_globalListen "renderUpdated" doRenderUpdated

    -- -- trigger a render now that the render callback is initialized
    -- js_globalShout "forceRender" (J.pToJSVal $ TD.App.renderSeqNum initialState)

    -- Start the App render
    root <- js_getElementById "root"
    e <- R.markedElement TD.App.window appModel
    RD.render (J.pToJSVal e) root

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies html React of any state changes.
    -- runEffect will only stop if input is finished (which in this example never does).
    void . P.runEffect $ appEffect appMModel output input

    -- Cleanup
    -- We actually never get here because in this example runEffect never quits
    -- but in other apps, gadgetEffect might be quit-able (eg with MaybeT)
    -- so let's add the cleanup code here to be explicit.
    -- J.releaseCallback doRender
    -- J.releaseCallback doRenderUpdated
    CD.dispose (CD.disposing appCallbacks)
    CD.dispose (CD.disposing inputCallbacks)

foreign import javascript unsafe
  "$r = document.getElementById($1);"
  js_getElementById :: J.JSString -> IO J.JSVal

-- foreign import javascript unsafe
--   "hgr$todo$registry['listen']($1, $2);"
--   js_globalListen :: J.JSString -> J.Callback a -> IO ()

-- foreign import javascript unsafe
--   "hgr$todo$registry['shout']($1, $2);"
--   js_globalShout :: J.JSString -> J.JSVal -> IO ()

appEffect
    :: MonadIO io
    => MVar TD.App.Model
    -> PC.Output TD.App.Action
    -> PC.Input TD.App.Action
    -> P.Effect io TD.App.Model
appEffect stateMVar output input = do
    s <- liftIO $ readMVar stateMVar
    PL.execStateP s $
        producerIO input P.>->
        interpretCommandsPipe stateMVar output P.>->
        PP.drain

producerIO :: MonadIO io => PC.Input TD.App.Action -> P.Producer' (D.DList TD.App.Command) (StateT TD.App.Model io) ()
producerIO input = hoist (hoist (liftIO . atomically)) (producer input)

producer :: PC.Input TD.App.Action -> P.Producer' (D.DList TD.App.Command) (StateT TD.App.Model STM) ()
producer input = PM.execInput input (G.runGadgetT TD.App.gadget)

interpretCommandsPipe
    :: (MonadState TD.App.Model io, MonadIO io)
    => MVar TD.App.Model
    -> PC.Output TD.App.Action
    -> P.Pipe (D.DList TD.App.Command) () io ()
interpretCommandsPipe stateMVar output = PP.mapM (interpretCommands stateMVar output)

interpretCommands
    :: (Foldable t, MonadState TD.App.Model io, MonadIO io)
    => MVar TD.App.Model
    -> PC.Output TD.App.Action
    -> t TD.App.Command
    -> io ()
interpretCommands stateMVar output =
    traverse_ (TD.App.Run.interpretCommand stateMVar output)
