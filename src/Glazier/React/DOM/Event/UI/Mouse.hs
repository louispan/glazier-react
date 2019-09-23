{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.DOM.Event.UI.Mouse
  ( NativeMouseEvent -- ^ constructor not exported
  , SyntheticMouseEvent -- ^ constructor not exported
  , IMouseEvent(..)
  )
where

import qualified GHCJS.Types as J
import Glazier.React.DOM.Event.UI
import Glazier.React.DOM.Event.UI.Mouse.Internal
import Glazier.React.DOM.EventTarget.Internal
import qualified JavaScript.Extras as JE

-- | Mouse and Drag/Drop events
-- https://facebook.github.io/react/docs/events.html#mouse-events
-- https://developer.mozilla.org/en-US/docs/Web/Events
-- Event names (eventType)
-- onClick (click) onContextMenu (contextmenu) onDoubleClick (dblclick)
-- onDrag (drag) onDragEnd (dragend) onDragEnter (dragenter) onDragExit (dragexit)
-- onDragLeave (dragleave) onDragOver (dragover) onDragStart (dragstart)
-- onDrop (drop) onMouseDown (mousedown) onMouseEnter (mouseenter) onMouseLeave (mouseleave)
-- onMouseMove (mousemove) onMouseOut (mouseout) onMouseOver (mouseover) onMouseUp (mouseup)
class IUIEvent j => IMouseEvent j where
  altKey :: j -> Bool
  altKey = js_altKey . JE.toJS

  button :: j -> Int
  button = js_button . JE.toJS

  buttons :: j -> Int
  buttons = js_buttons . JE.toJS

  clientX :: j -> Int
  clientX = js_clientX . JE.toJS

  clientY :: j -> Int
  clientY = js_clientY . JE.toJS

  ctrlKey :: j -> Bool
  ctrlKey = js_ctrlKey . JE.toJS

  getModifierState :: j -> J.JSString -> Bool
  getModifierState j n = js_getModifierState (JE.toJS j) n

  metaKey :: j -> Bool
  metaKey = js_metaKey . JE.toJS

  pageX :: j -> Int
  pageX = js_pageX . JE.toJS

  pageY :: j -> Int
  pageY = js_pageY . JE.toJS

  relatedTarget :: j -> Maybe EventTarget
  relatedTarget = JE.fromJS . js_relatedTarget . JE.toJS

  screenX :: j -> Int
  screenX = js_screenX . JE.toJS

  screenY :: j -> Int
  screenY = js_screenY . JE.toJS

  shiftKey :: j -> Bool
  shiftKey = js_shiftKey . JE.toJS

instance IMouseEvent NativeMouseEvent
instance IMouseEvent SyntheticMouseEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['altKey']"
    js_altKey :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['button']"
    js_button :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['buttons']"
    js_buttons :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['clientX']"
    js_clientX :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['clientY']"
    js_clientY :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['ctrlKey']"
    js_ctrlKey :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['getModifierState']($2)"
    js_getModifierState :: J.JSVal -> J.JSString -> Bool

foreign import javascript unsafe
    "$r = $1['metaKey']"
    js_metaKey :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1['pageX']"
    js_pageX :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['pageY']"
    js_pageY :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['relatedTarget']"
    js_relatedTarget :: J.JSVal -> J.JSVal

foreign import javascript unsafe
    "$r = $1['screenX']"
    js_screenX :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['screenY']"
    js_screenY :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['shiftKey']"
    js_shiftKey :: J.JSVal -> Bool

#else

js_altKey :: J.JSVal -> Bool
js_altKey _ = False

js_button :: J.JSVal -> Int
js_button _ = 0

js_buttons :: J.JSVal -> Int
js_buttons _ = 0

js_clientX :: J.JSVal -> Int
js_clientX _ = 0

js_clientY :: J.JSVal -> Int
js_clientY _ = 0

js_ctrlKey :: J.JSVal -> Bool
js_ctrlKey _ = False

js_getModifierState :: J.JSVal -> J.JSString -> Bool
js_getModifierState _ _ = False

js_metaKey :: J.JSVal -> Bool
js_metaKey _ = False

js_pageX :: J.JSVal -> Int
js_pageX _ = 0

js_pageY :: J.JSVal -> Int
js_pageY _ = 0

js_relatedTarget :: J.JSVal -> J.JSVal
js_relatedTarget _ = J.nullRef

js_screenX :: J.JSVal -> Int
js_screenX _ = 0

js_screenY :: J.JSVal -> Int
js_screenY _ = 0

js_shiftKey :: J.JSVal -> Bool
js_shiftKey _ = False

#endif
