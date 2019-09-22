{-# LANGUAGE CPP #-}

module Glazier.React.SyntheticEvent
  ( SyntheticEvent -- ^ constructor not exported
  , ISyntheticEvent(..)
  )
where

import qualified GHCJS.Types as J
import Glazier.React.DOM.Event.Internal
import Glazier.React.DOM.Event
import Glazier.React.SyntheticEvent.Internal
import qualified JavaScript.Extras as JE

-- | https://reactjs.org/docs/events.html
class IEvent j => ISyntheticEvent j where
    -- not supported natively
    isPropagationStopped :: j -> Bool
    isPropagationStopped = js_isPropagationStopped . JE.toJS

    nativeEvent :: j -> Event
    nativeEvent = Event . js_nativeEvent . JE.toJS

instance ISyntheticEvent SyntheticEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['nativeEvent']"
    js_nativeEvent :: J.JSVal -> J.JSVal

foreign import javascript unsafe
    "$r = $1['isPropagationStopped']"
    js_isPropagationStopped :: J.JSVal -> Bool

#else

js_isPropagationStopped :: J.JSVal -> Bool
js_isPropagationStopped _ = False

js_nativeEvent :: J.JSVal -> J.JSVal
js_nativeEvent _ = J.nullRef

#endif
