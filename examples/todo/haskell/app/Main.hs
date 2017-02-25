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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import Data.List
import Data.Monoid
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Prim as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Element as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Misc as PM
import qualified Pipes.Prelude as PP
import qualified Todo.App as TD.App
import qualified Todo.App.Run as TD.App.Run
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
main :: IO ()
main = do
    -- Create a 'Pipes.Concurrent' mailbox for receiving actions from html events.
    -- NB. using 'PC.bounded 1' also works without deadlocks, but doesn't save memory
    -- blocked events are kept by the GHCJCS runtime.
    (output, input) <- liftIO . PC.spawn $ PC.unbounded

    -- It is not trivial to call arbitrary Haskell functions from Javascript
    -- A hacky way is to create a Callback and assign it to a global registry.
    inputCallbacks <- TD.Input.mkCallbacks (TD.App.Run.mkActionCallback output . TD.App.mapInputHandler)
    appCallbacks <- TD.App.mkCallbacks (TD.App.Run.mkActionCallback output)

    -- TODO: How to make sure the correct handlers are passed into the correct place?
    let initialState = TD.App.Model
            "todos"
            0
            mempty
            0
            (TD.Input.Model
                 "new-input"
                 J.empty
                 inputCallbacks)
            mempty -- todosModel
            appCallbacks

        -- Make a MVar so render can get the latest state
    currentState <- newMVar initialState

    -- Setup the render callback
    -- render <- syncCallback1' (view G._WindowT' (jsval <$> counterWindow) . R.unsafeCoerceReactElement)
    doRender <- J.syncCallback1' $ \_ -> do
        s <- readMVar currentState
        xs <- view G._WindowT' (R.renderedWindow TD.App.window) s
        J.jsval <$> R.mkCombinedElements xs
    void $ js_globalListen "renderHaskell" doRender

    -- Setup the callback garbage collection callback
    doRenderUpdated <- J.asyncCallback1 $ \seqNum -> void . runMaybeT $ do
        seqNum' <- MaybeT $ J.fromJSVal seqNum
        MaybeT . fmap guard . atomically . PC.send output $ TD.App.RenderUpdatedAction seqNum'
    void $ js_globalListen "renderUpdated" doRenderUpdated

    -- trigger a render now that the render callback is initialized
    js_globalShout "forceRender" (J.pToJSVal $ TD.App.renderSeqNum initialState)

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies html React of any state changes.
    -- runEffect will only stop if input is finished (which in this example never does).
    void . P.runEffect $ appEffect currentState output input

    -- Cleanup
    -- We actually never get here because in this example runEffect never quits
    -- but in other apps, gadgetEffect might be quit-able (eg with MaybeT)
    -- so let's add the cleanup code here to be explicit.
    J.releaseCallback doRender
    J.releaseCallback doRenderUpdated
    CD.dispose (CD.disposing appCallbacks)
    CD.dispose (CD.disposing inputCallbacks)

foreign import javascript unsafe
  "hgr$todo$registry['listen']($1, $2);"
  js_globalListen :: J.JSString -> J.Callback a -> IO ()

foreign import javascript unsafe
  "hgr$todo$registry['shout']($1, $2);"
  js_globalShout :: J.JSString -> J.JSVal -> IO ()

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
interpretCommandsPipe stateMVar output = PP.mapM go
   where
     go cmds = do
         let (changes, cmds') = partition TD.App.isRenderRequiredCommand (D.toList cmds)
         interpretCommands stateMVar output cmds'
         -- Run only one of the state changed
         interpretCommands stateMVar output (foldMap (First . Just) changes)

interpretCommands
    :: (Foldable t, MonadState TD.App.Model io, MonadIO io)
    => MVar TD.App.Model
    -> PC.Output TD.App.Action
    -> t TD.App.Command
    -> io ()
interpretCommands stateMVar output =
    traverse_ (TD.App.Run.interpretCommand js_globalShout stateMVar output)
