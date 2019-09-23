{-# LANGUAGE CPP #-}

module Glazier.React.DOM.Event
    ( NativeEvent  -- ^ constructor is not exported
    , SyntheticEvent -- ^ constructor is not exported
    , IEvent(..)
    , ISyntheticEvent(..)
    )
    where

import Glazier.React.DOM.Event.Internal
import Glazier.React.DOM.EventTarget.Internal
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import Control.Monad.IO.Class

-- Subset of https://developer.mozilla.org/en-US/docs/Web/API/Event
-- Not providing @isPropagationStopped@ as it is not supported natively by JavaScript
class JE.ToJS j => IEvent j where
    bubbles :: j -> Bool
    bubbles = js_bubbles . JE.toJS

    cancelable :: j -> Bool
    cancelable = js_cancelable . JE.toJS

    currentTarget :: j -> EventTarget
    currentTarget = EventTarget . js_currentTarget . JE.toJS

    defaultPrevented  :: j -> Bool
    defaultPrevented = js_defaultPrevented . JE.toJS

    eventPhase :: j -> Int
    eventPhase = js_eventPhase . JE.toJS

    isTrusted :: j -> Bool
    isTrusted = js_isTrusted . JE.toJS

    target :: j -> EventTarget
    target = EventTarget . js_target . JE.toJS

    timeStamp :: j -> Int
    timeStamp = js_timeStamp . JE.toJS

    -- @type@ is a reserved word, so add a prefix to make 'eventType'
    eventType :: j -> J.JSString
    eventType = js_eventType . JE.toJS

    preventDefault :: MonadIO m => j -> m ()
    preventDefault = js_preventDefault . JE.toJS

    stopPropagation :: MonadIO m => j -> m ()
    stopPropagation = js_stopPropagation . JE.toJS

instance IEvent NativeEvent
instance IEvent SyntheticEvent

-- | https://reactjs.org/docs/events.html
class IEvent j => ISyntheticEvent j where
    isPropagationStopped :: j -> Bool
    isPropagationStopped = js_isPropagationStopped . JE.toJS

    nativeEvent :: j -> NativeEvent
    nativeEvent = NativeEvent . js_nativeEvent . JE.toJS

instance ISyntheticEvent SyntheticEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['bubbles']"
    js_bubbles :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['cancelable']"
    js_cancelable :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['currentTarget']"
    js_currentTarget :: J.JSVal -> J.JSVal

foreign import javascript unsafe
    "$1['preventDefault']()"
    js_preventDefault :: MonadIO m => J.JSVal -> m ()

foreign import javascript unsafe
    "$r = $1['eventPhase']"
    js_eventPhase :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['isTrusted']"
    js_isTrusted :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['target']"
    js_target :: J.JSVal -> J.JSVal

foreign import javascript unsafe
    "$r = $1['timeStamp']"
    js_timeStamp :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['type']"
    js_eventType :: J.JSVal -> J.JSString

foreign import javascript unsafe
    "$r = $1['defaultPrevented']"
    js_defaultPrevented :: J.JSVal -> Bool

foreign import javascript unsafe
    "$1['stopPropagation']()"
    js_stopPropagation :: MonadIO m => J.JSVal -> m ()

foreign import javascript unsafe
    "$r = $1['nativeEvent']"
    js_nativeEvent :: J.JSVal -> J.JSVal

foreign import javascript unsafe
    "$r = $1['isPropagationStopped']"
    js_isPropagationStopped :: J.JSVal -> Bool

#else

js_bubbles:: J.JSVal -> Bool
js_bubbles _ = False

js_cancelable:: J.JSVal -> Bool
js_cancelable _ = False

js_currentTarget :: J.JSVal -> J.JSVal
js_currentTarget _ = J.nullRef

js_defaultPrevented :: J.JSVal -> Bool
js_defaultPrevented _ = False

js_eventPhase :: J.JSVal -> Int
js_eventPhase _ = 0

js_isTrusted:: J.JSVal -> Bool
js_isTrusted _ = False

js_target :: J.JSVal -> J.JSVal
js_target _ = J.nullRef

js_timeStamp :: J.JSVal -> Int
js_timeStamp _ = 0

js_eventType :: J.JSVal -> J.JSString
js_eventType _ = mempty

js_preventDefault :: MonadIO m => J.JSVal -> m ()
js_preventDefault _ = pure ()

js_stopPropagation :: MonadIO m => J.JSVal -> m ()
js_stopPropagation _ = pure ()

js_isPropagationStopped :: J.JSVal -> Bool
js_isPropagationStopped _ = False

js_nativeEvent :: J.JSVal -> J.JSVal
js_nativeEvent _ = J.nullRef

#endif
