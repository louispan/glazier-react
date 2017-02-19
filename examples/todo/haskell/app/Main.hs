{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Monoid
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Prim as J
import qualified GHCJS.Types as J
import qualified Data.JSString as J
import qualified Glazier as G
import qualified Glazier.React.Element as R
import qualified Glazier.React.Markup as R
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Prelude as PP
import qualified Todo.App as TD
import qualified Todo.Input as TD.Input
import Control.Concurrent.MVar
import qualified Data.DList as D
import Data.List

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
main :: IO ()
main = do
    -- Create a 'Pipes.Concurrent' mailbox for receiving actions from html events.
    -- NB. using 'PC.bounded 1' also works without deadlocks, but doesn't save memory
    -- blocked events are kept by the GHCJCSruntime.
    (output, input) <- liftIO . PC.spawn $ PC.unbounded

    -- It is not trivial to call arbitrary Haskell functions from Javascript
    -- A hacky way is to create a Callback and assign it to a global registry.
    -- Input onChange callback
    doAppInputOnChange <- J.syncCallback1 J.ContinueAsync $ \evt -> void $ runMaybeT $ do
         action <- TD.appInputOnChange evt
         lift $ void $ atomically $ PC.send output action
    doAppInputOnKeyDown <- J.syncCallback1 J.ContinueAsync $ \evt -> void $ runMaybeT $ do
         action <- TD.appInputOnKeyDown evt
         lift $ void $ atomically $ PC.send output action

    let initialState = TD.AppModel (TD.Input.InputModel
                                    "input"
                                    "hello world!"
                                    doAppInputOnChange
                                    doAppInputOnKeyDown
                                   )

    -- Make a MVar so render can get the latest state
    currentState <- newMVar initialState

    -- Setup the render callback
    -- render <- syncCallback1' (view G._WindowT' (jsval <$> counterWindow) . R.unsafeCoerceReactElement)
    doRender <- J.syncCallback1' $ \_ -> do
        s <- readMVar currentState
        xs <- view G._WindowT' (R.renderedWindow TD.appWindow) s
        J.jsval <$> R.mkCombinedElements xs
    void $ js_globalListen "renderHaskell" doRender

    -- trigger a render now that the render callback is initialized
    js_globalShout "forceRender" J.nullRef

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies html React of any state changes.
    -- runEffect will only stop if input is finished (which in this example never does).
    void . P.runEffect $ appEffect currentState input

    -- Cleanup
    -- We actually never get here because in this example runEffect never quits
    -- but in other apps, gadgetEffect might be quit-able (eg with MaybeT)
    -- so let's add the cleanup code here to be explicit.
    J.releaseCallback doAppInputOnChange
    J.releaseCallback doRender

foreign import javascript unsafe
  "hgr$todo$registry['listen']($1, $2);"
  js_globalListen :: J.JSString -> J.Callback a -> IO ()

foreign import javascript unsafe
  "hgr$todo$registry['shout']($1, $2);"
  js_globalShout :: J.JSString -> J.JSVal -> IO ()

interpretCommand :: (MonadIO io, MonadState TD.AppModel io) => MVar TD.AppModel -> TD.AppCommand -> io ()
interpretCommand stateMVar TD.AppStateChangedCommand = do
    s <- get
    liftIO . void $ swapMVar stateMVar s -- ^ so that the render callback can use the latest state
    liftIO $ js_globalShout "forceRender" J.nullRef -- ^ tell React to call render

interpretCommand _ (TD.AppTodoEnteredCommand str) = do
    liftIO $ putStrLn $ "TODO entered: " ++ (J.unpack str)

interpretCommands
    :: (Foldable t, MonadState TD.AppModel io, MonadIO io)
    => MVar TD.AppModel -> t TD.AppCommand -> io ()
interpretCommands stateMVar = traverse_ (interpretCommand stateMVar)

interpretCommandsPipe
    :: (MonadState TD.AppModel io, MonadIO io)
    => MVar TD.AppModel -> P.Pipe (D.DList TD.AppCommand) () io ()
interpretCommandsPipe stateMVar = PP.mapM go
   where
     isStateChangedCmd cmd = case cmd of
         TD.AppStateChangedCommand -> True
         _ -> False
     go cmds = do
         -- Run only one of the state changed command last
         let (changes, cmds') = partition isStateChangedCmd (D.toList cmds)
         interpretCommands stateMVar cmds'
         interpretCommands stateMVar (foldMap (First . Just) changes)

appEffect :: MonadIO io => MVar TD.AppModel -> PC.Input TD.AppAction -> P.Effect io TD.AppModel
appEffect stateMVar input = do
    s <- liftIO $ readMVar stateMVar
    PL.execStateP s $ TD.appProducer input P.>-> interpretCommandsPipe stateMVar P.>-> PP.drain
