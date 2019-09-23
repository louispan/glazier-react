{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.DOM.Event.HashChange
  ( HashChangeEvent(..)
  , toHashChangeEvent
  )
where

import Control.DeepSeq
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import Glazier.React.DOM.Event.Internal
import Glazier.React.DOM.Event.Notice.Internal
import Glazier.React.DOM.EventTarget.Internal
import qualified JavaScript.Extras as JE

-- https://developer.mozilla.org/en-US/docs/Web/API/HashChangeEvent
data HashChangeEvent = HashChangeEvent
  { target :: EventTarget
  -- type is a reserved word, so prefix to eventType
  , eventType :: J.JSString
  , bubbles :: Bool
  , cancelable :: Bool
  , oldURL ::J.JSString
  , newURL :: J.JSString
  }
    deriving (G.Generic)
instance NFData HashChangeEvent

-- | We can lie about this not being in IO because
-- within the strict part of 'handleEventM'
-- the Notice is effectively immutable.
toHashChangeEvent :: Event -> Maybe HashChangeEvent
toHashChangeEvent nevt | js_isHashChangeEvent evt = Just $
    HashChangeEvent
    { target = EventTarget $ unsafeGetProperty evt "target"
    , eventType = unsafeGetProperty evt "type"
    , bubbles = unsafeGetProperty evt "bubbles"
    , cancelable = unsafeGetProperty evt "cancelable"
    , oldURL = unsafeGetProperty evt "oldURL"
    , newURL = unsafeGetProperty evt "newURL"
    }
  where evt = JE.toJS nevt
toHashChangeEvent _ | otherwise = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof HashChangeEvent"
    js_isHashChangeEvent :: J.JSVal -> Bool

#else

js_isHashChangeEvent :: J.JSVal -> Bool
js_isHashChangeEvent _ = False

#endif
