{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.State.Strict
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Prim as J
import qualified Glazier as G
import qualified Glazier.React.Maker.Run as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.ReactDOM as RD
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Misc as PM
import qualified Pipes.Prelude as PP
import qualified Todo.App as TD.App
import qualified Todo.App.Run as TD.App.Run

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

    -- App Model
    (ms, s) <- liftIO $ iterM (R.runMaker output) (TD.App.mkMModel "todos")

    -- Start the App render
    root <- js_getElementById "root"
    e <- R.markedElement TD.App.window s
    RD.render (J.pToJSVal e) root

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies html React of any state changes.
    -- runEffect will only stop if input is finished (which in this example never does).
    s' <- P.runEffect $ appEffect ms output input

    -- Cleanup
    -- We actually never get here because in this example runEffect never quits
    -- but in other apps, gadgetEffect might be quit-able (eg with MaybeT)
    -- so let's add the cleanup code here to be explicit.
    CD.dispose (CD.disposing s')

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
    => MVar TD.App.CModel
    -> PC.Output TD.App.Action
    -> PC.Input TD.App.Action
    -> P.Effect io TD.App.CModel
appEffect stateMVar output input = do
    s <- liftIO $ readMVar stateMVar
    PL.execStateP s $
        producerIO input P.>->
        interpretCommandsPipe stateMVar output P.>->
        PP.drain

producerIO :: MonadIO io => PC.Input TD.App.Action -> P.Producer' (D.DList TD.App.Command) (StateT TD.App.CModel io) ()
producerIO input = hoist (hoist (liftIO . atomically)) (producer input)

producer :: PC.Input TD.App.Action -> P.Producer' (D.DList TD.App.Command) (StateT TD.App.CModel STM) ()
producer input = PM.execInput input (G.runGadgetT (zoom TD.App.model TD.App.gadget))

interpretCommandsPipe
    :: (MonadState TD.App.CModel io, MonadIO io)
    => MVar TD.App.CModel
    -> PC.Output TD.App.Action
    -> P.Pipe (D.DList TD.App.Command) () io ()
interpretCommandsPipe stateMVar output = PP.mapM (interpretCommands stateMVar output)

interpretCommands
    :: (Foldable t, MonadState TD.App.CModel io, MonadIO io)
    => MVar TD.App.CModel
    -> PC.Output TD.App.Action
    -> t TD.App.Command
    -> io ()
interpretCommands stateMVar output =
    traverse_ (TD.App.Run.interpretCommand stateMVar output)
