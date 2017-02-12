{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.State.Strict
import Data.Foldable
import Data.Monoid
import GHCJS.Foreign.Callback
    ( Callback
    , OnBlocked(..)
    , releaseCallback
    , syncCallback1
    , syncCallback1'
    )
import GHCJS.Nullable (Nullable(..), nullableToMaybe)
import GHCJS.Prim (toJSInt)
import GHCJS.Types (JSString, JSVal, jsval, nullRef)
import qualified Glazier as G
import qualified Glazier.Pipes.Strict as GP
import qualified Glazier.React.Element as R
import qualified Glazier.React.Event as R
import JavaScript.Array (fromList)
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as PP
import qualified Pipes.Lift as PL

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
main :: IO ()
main = do
    -- cb <- syncCallback1 ContinueAsync $ R.mkEventHandler
    --     (fmap (R.eventType . R.parseEvent) . R.castSyntheticEvent)
    --     (void . sequenceA . fmap T.putStrLn)
    -- void $ js_globalAssignCallback "cb" cb

    -- Create a 'Pipes.Concurrent' mailbox for receiving actions from events
    (output, input) <- liftIO . PC.spawn $ PC.unbounded

    onIncrement <- syncCallback1 ContinueAsync $ R.mkEventHandler
         (const ()) -- don't need any data from the event, just that it happened
         (void . atomically . PC.send output . const Increment)

    -- It is not trivial to call arbitrary Haskell functions from Javascript
    -- A hacky way is to create a Callback and assign it to a global.
    -- In this example app, the webpage is always being rendered so the callback is not released.
    void $ js_globalAssignCallback "onIncrement" onIncrement

    onDecrement <- syncCallback1 ContinueAsync $ R.mkEventHandler
         (const ()) -- don't need any data from the event, just that it happened
         (void . atomically . PC.send output . const Decrement)
    void $ js_globalAssignCallback "onDecrement" onDecrement

    -- This useless plubming is to test that React.Component.setState is only called
    -- if the state actually changes.
    onIgnore <- syncCallback1 ContinueAsync $ R.mkEventHandler
         (const ()) -- don't need any data from the event, just that it happened
         (void . atomically . PC.send output . const Ignore)
    void $ js_globalAssignCallback "onIgnore" onDecrement

    -- Setup the render callback
    renderCb <- syncCallback1' (view G._Window' (jsval <$> counterWindow))
    void $ js_globalAssignCallback "render" renderCb

    -- trigger a render now that the render callback is initialized
    let initialState = (review _JSInt 0)
    js_globalNotifyListeners "counterState" initialState

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies React of any state changes.
    -- This will only stop if input is finished.
    P.runEffect $ gadgetEffect initialState input

    -- Cleanup
    -- Wwe actually never get here because in this example runEffect never quits
    -- but in other apps, interpretCommandsPipe might be quit-able (MaybeT)
    -- so let's add the cleanup code here to be explicit.
    releaseCallback onIncrement
    releaseCallback onDecrement
    releaseCallback onIgnore
    releaseCallback renderCb

foreign import javascript unsafe
  "h$glazier$react$todo[$1] = $2;"
  js_globalAssignCallback :: JSString -> Callback a -> IO ()

foreign import javascript unsafe
  "h$glazier$react$todo.notifyListeners($1, $2);"
  js_globalNotifyListeners :: JSString -> JSVal -> IO ()

_JSInt :: Prism' JSVal Int
_JSInt = prism' toJSInt (nullableToMaybe . Nullable)

counterWindow :: Applicative m => G.Window m JSVal R.ReactElement
counterWindow = review G._Window $ \a -> pure $ R.createElement "div" nullRef (fromList [a])

data CounterAction = Increment | Decrement | Ignore -- used to test that we don't re-render
    deriving (Show)

-- | A simple gadget using Haskell Int as the state
counterGadget :: Applicative m => G.Gadget Int m CounterAction (Maybe StateChangedCommand)
counterGadget = review G._Gadget $ \a s -> case a of
    Increment -> pure (Just StateChanged, s + 1)
    Decrement -> pure (Just StateChanged, s - 1)
    Ignore -> pure (Nothing, s)

-- | Implant the Haskell state into JSVal
-- This is so that React can use shallow comparison of JSVal to avoid re-rendering
-- if state has not changed
counterGadget' :: Monad m => G.Gadget JSVal m CounterAction (Maybe StateChangedCommand)
counterGadget' = getFirst <$> (G.implant _JSInt (First <$> counterGadget))

-- | If state changed, then run the notifyListeners IO action
data StateChangedCommand = StateChanged

notifyStateChanged :: (MonadIO io, MonadState JSVal io) => StateChangedCommand -> io ()
notifyStateChanged StateChanged = do
    s <- get
    liftIO $ js_globalNotifyListeners "counterState" s

notifyStateChanged' :: (Foldable t, MonadState JSVal io, MonadIO io) => t StateChangedCommand -> io ()
notifyStateChanged' = traverse_ notifyStateChanged

interpretCommandsPipe :: (MonadState JSVal io, MonadIO io) => P.Pipe (Maybe StateChangedCommand) () io ()
interpretCommandsPipe = PP.mapM notifyStateChanged'

gadgetProducer :: (MFunctor t, MonadState JSVal (t STM), MonadTrans t, MonadIO io) => PC.Input CounterAction -> P.Producer' (Maybe StateChangedCommand) (t io) ()
gadgetProducer input = hoist (hoist (liftIO . atomically)) (GP.gadgetToProducer input counterGadget')

gadgetEffect :: MonadIO io => JSVal -> PC.Input CounterAction -> P.Effect io ()
gadgetEffect s input = PL.evalStateP s $ gadgetProducer input P.>-> interpretCommandsPipe P.>-> PP.drain
