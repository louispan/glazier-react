{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Event.Mouse
  ( MouseEvent(..)
  , toMouseEvent
  )
where

import Control.DeepSeq
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import Glazier.React.EventTarget.Internal
import Glazier.React.Notice.Internal

-- | Mouse and Drag/Drop events
-- 'MouseEvent' must only be used in the first part of 'handleEvent'.
-- https://facebook.github.io/react/docs/events.html#mouse-events
-- https://developer.mozilla.org/en-US/docs/Web/Events
-- Event names (eventType)
-- onClick (click) onContextMenu (contextmenu) onDoubleClick (dblclick)
-- onDrag (drag) onDragEnd (dragend) onDragEnter (dragenter) onDragExit (dragexit)
-- onDragLeave (dragleave) onDragOver (dragover) onDragStart (dragstart)
-- onDrop (drop) onMouseDown (mousedown) onMouseEnter (mouseenter) onMouseLeave (mouseleave)
-- onMouseMove (mousemove) onMouseOut (mouseout) onMouseOver (mouseover) onMouseUp (mouseup)
data MouseEvent = MouseEvent
  { altKey :: Bool
  , button :: Int
  , buttons :: Int
  , clientX :: Int
  , clientY :: Int
  , ctrlKey :: Bool
  , getModifierState :: J.JSString -> Bool
  , metaKey :: Bool
  , pageX :: Int
  , pageY :: Int
  , relatedTarget :: EventTarget
  , screenX :: Int
  , screenY :: Int
  , shiftKey :: Bool
  }
    deriving (G.Generic)
instance NFData MouseEvent

-- | We can lie about this not being in IO because
-- within the strict part of 'handleEventM'
-- the Notice is effectively immutable.
toMouseEvent :: Notice -> Maybe MouseEvent
toMouseEvent (Notice evt) | js_isMouseEvent (unsafeGetProperty evt "nativeEvent") = Just $
    MouseEvent
    { altKey = unsafeGetProperty evt "altKey"
    , button = unsafeGetProperty evt "button"
    , buttons = unsafeGetProperty evt "buttons"
    , clientX = unsafeGetProperty evt "clientX"
    , clientY = unsafeGetProperty evt "clientY"
    , ctrlKey = unsafeGetProperty evt "ctrlKey"
    , getModifierState = unsafeGetModifierState evt
    , metaKey = unsafeGetProperty evt "metaKey"
    , pageX = unsafeGetProperty evt "pageX"
    , pageY = unsafeGetProperty evt "pageY"
    , relatedTarget = EventTarget $ unsafeGetProperty evt "relatedTarget"
    , screenX = unsafeGetProperty evt "screenX"
    , screenY = unsafeGetProperty evt "xcreenY"
    , shiftKey = unsafeGetProperty evt "shiftKey"
    }
toMouseEvent _ | otherwise = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "($1 instanceof MouseEvent)"
    js_isMouseEvent :: J.JSVal -> Bool

#else

js_isMouseEvent :: J.JSVal -> Bool
js_isMouseEvent _ = False

#endif
