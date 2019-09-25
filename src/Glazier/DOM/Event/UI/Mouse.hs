{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.DOM.Event.UI.Mouse
  ( NativeMouseEvent -- ^ constructor not exported
  , SyntheticMouseEvent -- ^ constructor not exported
  , IMouseEvent(..)
  )
where

import qualified GHCJS.Types as J
import Glazier.DOM.Event.UI
import Glazier.DOM.Event.UI.Keyboard
import Glazier.DOM.Event.UI.Mouse.Internal
import Glazier.DOM.EventTarget.Internal
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
class ICommonKeyboardEvent j => IMouseEvent j where
  button :: j -> Int
  button = js_button . JE.toJS

  buttons :: j -> Int
  buttons = js_buttons . JE.toJS

  clientX :: j -> Int
  clientX = js_clientX . JE.toJS

  clientY :: j -> Int
  clientY = js_clientY . JE.toJS

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

instance IMouseEvent NativeMouseEvent
instance IMouseEvent SyntheticMouseEvent

#ifdef __GHCJS__

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

#else

js_button :: J.JSVal -> Int
js_button _ = 0

js_buttons :: J.JSVal -> Int
js_buttons _ = 0

js_clientX :: J.JSVal -> Int
js_clientX _ = 0

js_clientY :: J.JSVal -> Int
js_clientY _ = 0

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

#endif
