{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.DOM.Event.Keyboard
  ( KeyboardEvent(..)
  , toKeyboardEvent
  )
where

import Control.DeepSeq
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import Glazier.React.DOM.Event.Notice.Internal
import Prelude hiding (repeat)

-- | Keyboard events
-- 'KeyboardEvent' must only be used in the first part of 'handleEvent'.
-- https://facebook.github.io/react/docs/events.html#keyboard-events
-- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
-- Event names (eventType)
-- onKeyDown (keydown) onKeyPress (keypress) onKeyUp (keyyp)
data KeyboardEvent = KeyboardEvent
  { altKey :: Bool
  , charCode ::Int
  , ctrlKey :: Bool
  , getModifierState :: J.JSString -> Bool
  , key :: J.JSString
  , keyCode :: Int
  , locale :: J.JSString
  , location ::Int
  , metaKey :: Bool
  , repeat :: Bool
  , shiftkey :: Bool
  , which :: Int
  }
    deriving (G.Generic)
instance NFData KeyboardEvent

-- | We can lie about this not being in IO because
-- within the strict part of 'handleEventM'
-- the Notice is effectively immutable.
toKeyboardEvent :: Notice -> Maybe KeyboardEvent
toKeyboardEvent (Notice evt) | js_isKeyboardEvent (unsafeGetProperty evt "nativeEvent") = Just $
    KeyboardEvent
    { altKey = unsafeGetProperty evt "altKey"
    , charCode = unsafeGetProperty evt "charCode"
    , ctrlKey = unsafeGetProperty evt "ctrlKey"
    , getModifierState = unsafeGetModifierState evt
    , key = unsafeGetProperty evt "key"
    , keyCode = unsafeGetProperty evt "keyCode"
    , locale = unsafeGetProperty evt "locale"
    , location = unsafeGetProperty evt "location"
    , metaKey = unsafeGetProperty evt "metaKey"
    , repeat = unsafeGetProperty evt "repeat"
    , shiftkey = unsafeGetProperty evt "shiftkey"
    , which = unsafeGetProperty evt "which"
    }
toKeyboardEvent _ | otherwise = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "($1 instanceof KeyboardEvent)"
    js_isKeyboardEvent :: J.JSVal -> Bool

#else

js_isKeyboardEvent :: J.JSVal -> Bool
js_isKeyboardEvent _ = False

#endif
