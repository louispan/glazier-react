{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Prim as J
import qualified Glazier as G
import qualified Glazier.React.Maker.Run as R.Maker
import qualified Glazier.React.Markup as R
import qualified Glazier.React.ReactDOM as RD
import qualified Glazier.React.Model.Class as R
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Misc as PM
import qualified Pipes.Prelude as PP
import qualified Todo.App as TD.App
import qualified Todo.App.Run as TD.App

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
    s <- liftIO $ iterM (R.Maker.run output) (TD.App.mkSuperModel "todos")

    -- Start the App render
    root <- js_getElementById "root"
    e <- R.markedElement TD.App.window (s ^. R.cModel)
    RD.render (J.pToJSVal e) root

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies html React of any state changes.
    -- runEffect will only stop if input is finished (which in this example never does).
    s' <- P.runEffect $ appEffect s output input

    -- Cleanup
    -- We actually never get here because in this example runEffect never quits
    -- but in other apps, gadgetEffect might be quit-able (eg with MaybeT)
    -- so let's just be explicit where cleanup code would be.
    CD.dispose (CD.disposing s')

foreign import javascript unsafe
  "$r = document.getElementById($1);"
  js_getElementById :: J.JSString -> IO J.JSVal

appEffect
    :: MonadIO io
    => TD.App.SuperModel
    -> PC.Output TD.App.Action
    -> PC.Input TD.App.Action
    -> P.Effect io TD.App.SuperModel
appEffect s output input = do
    PL.execStateP s $
        appProducerIO input P.>->
        runCommandsPipe output P.>->
        PP.drain

appProducerIO :: MonadIO io => PC.Input TD.App.Action -> P.Producer' (D.DList TD.App.Command) (StateT TD.App.SuperModel io) ()
appProducerIO input = hoist (hoist (liftIO . atomically)) (appProducer input)

appProducer :: PC.Input TD.App.Action -> P.Producer' (D.DList TD.App.Command) (StateT TD.App.SuperModel STM) ()
appProducer input = PM.execInput input (G.runGadgetT TD.App.gadget)

runCommandsPipe
    :: (MonadState TD.App.SuperModel io, MonadIO io)
    => PC.Output TD.App.Action
    -> P.Pipe (D.DList TD.App.Command) () io ()
runCommandsPipe output = PP.mapM (runCommands output)

runCommands
    :: (Foldable t, MonadState TD.App.SuperModel io, MonadIO io)
    => PC.Output TD.App.Action
    -> t TD.App.Command
    -> io ()
runCommands output =
    traverse_ (\cmd -> runReaderT (TD.App.run cmd) (TD.App.Env output id))
