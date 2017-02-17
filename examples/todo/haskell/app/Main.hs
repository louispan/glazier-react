{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.STM
import qualified Data.HashMap.Strict as M
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
import GHCJS.Types (JSString, JSVal, jsval)
import qualified Glazier as G
import qualified Glazier.React.Element as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Event as R
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Prelude as PP
import qualified Pipes.Misc as PM

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
main :: IO ()
main = do
    -- Create a 'Pipes.Concurrent' mailbox for receiving actions from html events.
    -- NB. using 'PC.bounded 1' also works without deadlocks, but doesn't save memory
    -- blocked events are kept by the GHCJCSruntime.
    (output, input) <- liftIO . PC.spawn $ PC.unbounded

    onIncrement <- syncCallback1 ContinueAsync $ R.eventHandler
         (const ()) -- don't need any data from the event, just that it happened
         (void . atomically . PC.send output . const Increment)

    -- It is not trivial to call arbitrary Haskell functions from Javascript
    -- A hacky way is to create a Callback and assign it to a global.
    void $ js_globalListen "onIncrement" onIncrement

    onDecrement <- syncCallback1 ContinueAsync $ R.eventHandler
         (const ()) -- don't need any data from the event, just that it happened
         (void . atomically . PC.send output . const Decrement)
    void $ js_globalListen "onDecrement" onDecrement

    -- This useless event handler is to test that React.Component.setState is only called
    -- if the state actually changes.
    onIgnore <- syncCallback1 ContinueAsync $ R.eventHandler
         (const ()) -- don't need any data from the event, just that it happened
         (void . atomically . PC.send output . const Ignore)
    void $ js_globalListen "onIgnore" onIgnore

    -- Setup the render callback
    -- render <- syncCallback1' (view G._WindowT' (jsval <$> counterWindow) . R.unsafeCoerceReactElement)
    render <- syncCallback1' $ \a -> do
        let s = R.unsafeCoerceReactElement a
        es <- view G._WindowT' (sequenceA [counterWindow, counterWindow'']) s
        jsval <$> R.combineReactElements es
    void $ js_globalListen "render" render

    -- trigger a render now that the render callback is initialized
    let initialState = (review _JSInt 0)
    js_globalShout "counterState" initialState

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies html React of any state changes.
    -- runEffect will only stop if input is finished (which in this example never does).
    s <- P.runEffect $ gadgetEffect initialState input

    -- Cleanup
    -- We actually never get here because in this example runEffect never quits
    -- but in other apps, gadgetEffect might be quit-able (MaybeT)
    -- so let's add the cleanup code here to be explicit.
    releaseCallback onIncrement
    releaseCallback onDecrement
    releaseCallback onIgnore
    releaseCallback render
    js_printFinalState s

foreign import javascript unsafe
  "hgr$todo$registry['listen']($1, $2);"
  js_globalListen :: JSString -> Callback a -> IO ()

foreign import javascript unsafe
  "hgr$todo$registry['shout']($1, $2);"
  js_globalShout :: JSString -> JSVal -> IO ()

foreign import javascript unsafe
  "console.log('Final state', $1);"
  js_printFinalState :: JSVal -> IO ()

_JSInt :: Prism' JSVal Int
_JSInt = prism' toJSInt (nullableToMaybe . Nullable)

-- | Example of using 'ReactElement' directly
counterWindow :: MonadIO io => G.WindowT R.ReactElement io R.ReactElement
counterWindow = review G._WindowT $ \a -> liftIO $ R.createReactElement "div" M.empty [a]

-- | Example of using 'R.ReactMlt' transformer
counterWindow' :: Monad m => G.WindowT R.ReactElement (R.ReactMlT m) ()
counterWindow' = review G._WindowT $ \content ->
    R.branch "div" M.empty $ do
        (R.fromReactElement content)
        (R.fromReactElement content)

counterWindow'' :: MonadIO io => G.WindowT R.ReactElement io R.ReactElement
counterWindow'' = G.belowWindowT (R.toReactElement .) counterWindow'

data CounterAction = Increment | Decrement | Ignore -- used to test that we don't re-render
    deriving (Show)

-- | A simple gadget using Haskell Int as the state
counterGadget :: Applicative m => G.GadgetT CounterAction Int m (Maybe StateChangedCommand)
counterGadget = review G._GadgetT $ \a s -> case a of
    Increment -> pure (Just StateChanged, s + 1)
    Decrement -> pure (Just StateChanged, s - 1)
    Ignore -> pure (Nothing, s)

-- | Implant the Haskell state into JSVal
-- This is so that React can use shallow comparison of JSVal to avoid re-rendering
-- if state has not changed
counterGadget' :: Monad m => G.GadgetT CounterAction JSVal m (Maybe StateChangedCommand)
counterGadget' = getFirst <$> (G.implant _JSInt (First <$> counterGadget))

-- | If state changed, then run the notifyListeners IO action
data StateChangedCommand = StateChanged

notifyStateChanged :: (MonadIO io, MonadState JSVal io) => StateChangedCommand -> io ()
notifyStateChanged StateChanged = do
    s <- get
    liftIO $ js_globalShout "counterState" s

notifyStateChanged' :: (Foldable t, MonadState JSVal io, MonadIO io) => t StateChangedCommand -> io ()
notifyStateChanged' = traverse_ notifyStateChanged

interpretCommandsPipe :: (MonadState JSVal io, MonadIO io) => P.Pipe (Maybe StateChangedCommand) () io ()
interpretCommandsPipe = PP.mapM notifyStateChanged'

gadgetProducer
    :: (MFunctor t, MonadState JSVal (t STM), MonadTrans t, MonadIO io)
    => PC.Input CounterAction
    -> P.Producer' (Maybe StateChangedCommand) (t io) ()
gadgetProducer input = hoist (hoist (liftIO . atomically)) (PM.rsProducer input (G.runGadgetT counterGadget'))

gadgetEffect :: MonadIO io => JSVal -> PC.Input CounterAction -> P.Effect io JSVal
gadgetEffect s input = PL.execStateP s $ gadgetProducer input P.>-> interpretCommandsPipe P.>-> PP.drain
