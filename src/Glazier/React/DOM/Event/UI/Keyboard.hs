{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.DOM.Event.UI.Keyboard
  ( NativeKeyboardEvent -- ^ constructor not exported
  , SyntheticKeyboardEvent -- ^ constructor not exported
  , IKeyboardEvent(..)
  )
where

import qualified GHCJS.Types as J
import Glazier.React.DOM.Event.UI
import Glazier.React.DOM.Event.UI.Keyboard.Internal
import qualified JavaScript.Extras as JE

-- | Keyboard events
-- https://facebook.github.io/react/docs/events.html#keyboard-events
-- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
-- Event names (eventType)
-- onKeyDown (keydown) onKeyPress (keypress) onKeyUp (keyyp)
class IUIEvent j => IKeyboardEvent j where
  altKey :: j -> Bool
  altKey = js_altKey . JE.toJS

  charCode :: j -> Int
  charCode = js_charCode . JE.toJS

  ctrlKey :: j -> Bool
  ctrlKey = js_ctrlKey . JE.toJS

  getModifierState :: j -> J.JSString -> Bool
  getModifierState j n = js_getModifierState (JE.toJS j) n

  key :: j -> J.JSString
  key = js_key . JE.toJS

  keyCode :: j -> Int
  keyCode = js_keyCode . JE.toJS

  locale :: j -> J.JSString
  locale = js_locale . JE.toJS

  location :: j -> Int
  location = js_location . JE.toJS

  metaKey :: j -> Bool
  metaKey = js_metaKey . JE.toJS

  repeat :: j -> Bool
  repeat = js_repeat . JE.toJS

  shiftKey :: j -> Bool
  shiftKey = js_shiftKey . JE.toJS

  which :: j -> Int
  which = js_which . JE.toJS

instance IKeyboardEvent NativeKeyboardEvent
instance IKeyboardEvent SyntheticKeyboardEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['altKey']"
    js_altKey :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['charCode']"
    js_charCode :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['ctrlKey']"
    js_ctrlKey :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['getModifierState']($2)"
    js_getModifierState :: J.JSVal -> J.JSString -> Bool

foreign import javascript unsafe
    "$r = $1['key']"
    js_key :: J.JSVal -> J.JSString

foreign import javascript unsafe
    "$r = $1['keyCode']"
    js_keyCode :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['locale']"
    js_locale :: J.JSVal -> J.JSString

foreign import javascript unsafe
    "$r = $1['location']"
    js_location :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['metaKey']"
    js_metaKey :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['repeat']"
    js_repeat :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['shiftKey']"
    js_shiftKey :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['which']"
    js_which :: J.JSVal -> Int

#else

js_altKey :: j -> Bool
js_altKey _ = False

js_charCode :: j -> Int
js_charCode _ = 0

js_ctrlKey :: j -> Bool
js_ctrlKey _ = False

js_getModifierState :: j -> J.JSString -> Bool
js_getModifierState _ _ = False

js_key :: j -> J.JSString
js_key _ = mempty

js_keyCode :: j -> Int
js_keyCode _ = 0

js_locale :: j -> J.JSString
js_locale _ = mempty

js_location :: j -> Int
js_location _ = 0

js_metaKey :: j -> Bool
js_metaKey _ = False

js_repeat :: j -> Bool
js_repeat _ = False

js_shiftKey :: j -> Bool
js_shiftKey _ = False

js_which :: j -> Int
js_which _ = 0

#endif
