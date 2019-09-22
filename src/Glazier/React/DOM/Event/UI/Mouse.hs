{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.DOM.Event.Mouse
  ( MouseEvent -- ^ constructor not exported
  , IMouseEvent(..)
  )
where

import Glazier.React.DOM.Event.Mouse.Internal
import Glazier.React.DOM.EventTarget.Internal
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import Control.Monad.IO.Class

class IEvent j => IMouseEvent j where
  altKey :: Bool
  button :: Int
  buttons :: Int
  clientX :: Int
  clientY :: Int
  ctrlKey :: Bool
  getModifierState :: J.JSString -> Bool
  metaKey :: Bool
  pageX :: Int
  pageY :: Int
  relatedTarget :: EventTarget
  screenX :: Int
  screenY :: Int
  shiftKey :: Bool


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
