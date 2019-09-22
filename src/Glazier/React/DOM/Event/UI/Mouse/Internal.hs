{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.Event.Mouse.Internal
( MouseEvent(..) -- ^ constructor is exported
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
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
newtype MouseEvent =
    MouseEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS MouseEvent where
    validInstance = js_isMouseEvent
    fromJS a | js_isEvent a = Just $ MouseEvent a
    fromJS _ = Nothing


#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 instanceof MouseEvent"
    js_isMouseEvent :: J.JSVal -> Bool

#else

js_isMouseEvent :: J.JSVal -> Bool
js_isMouseEvent _ = False

#endif
