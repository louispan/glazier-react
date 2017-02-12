{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module based on React/Flux/PropertiesAndEvents.hs.
module Glazier.React.Event
  ( DOMEventTarget
  , DOMEvent
  , SyntheticEvent
  , castSyntheticEvent
  , mkEventHandler
  , Event(..)
  -- , HasEvent(..)
  , js_preventDefault
  , js_isDefaultPrevented
  , js_stopPropagation
  , js_isPropagationStopped
  , parseEvent
  , ModifierKey(..)
  , MouseEvent(..)
  , parseMouseEvent
  )
where

-- import Control.Lens
import Control.DeepSeq
import Data.JSString (pack)
import qualified Data.Text as T
import GHCJS.Foreign (fromJSBool)
import GHCJS.Marshal.Pure (PFromJSVal(..), PToJSVal(..))
import GHCJS.Types (IsJSVal, JSString, JSVal, jsval)
import JavaScript.Cast (Cast(..))

-- | The object that dispatched the event.
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
newtype DOMEventTarget = DOMEventTarget JSVal

instance IsJSVal DOMEventTarget
instance PToJSVal DOMEventTarget where
    pToJSVal = jsval

instance Cast DOMEventTarget where
    unsafeWrap = DOMEventTarget
    instanceRef _ = js_DOMEventTarget

foreign import javascript unsafe
    "EventTarget"
    js_DOMEventTarget :: JSVal

-- | The native event
-- https://developer.mozilla.org/en-US/docs/Web/API/Event
newtype DOMEvent = DOMEvent JSVal

instance IsJSVal DOMEvent
instance PToJSVal DOMEvent where
    pToJSVal = jsval

instance Cast DOMEvent where
    unsafeWrap = DOMEvent
    instanceRef _ = js_DOMEvent

foreign import javascript unsafe
    "Event"
    js_DOMEvent :: JSVal

-- | Every event in React is a synthetic event, a cross-browser wrapper around the native event.
-- FIXME: protect SyntheticEvent so that it can only be constructed on SyntheticEvent
newtype SyntheticEvent = SyntheticEvent JSVal

instance IsJSVal SyntheticEvent
instance PToJSVal SyntheticEvent where
    pToJSVal = jsval

foreign import javascript unsafe
    "($1 && $1.nativeEvent && $1.nativeEvent instanceof Event)"
    js_isSyntheticEvent :: JSVal -> Bool

-- | SyntheticEvent cannot be a Javascript.Cast
-- See https://github.com/ghcjs/ghcjs-base/issues/86
castSyntheticEvent :: JSVal -> Maybe SyntheticEvent
castSyntheticEvent a | js_isSyntheticEvent a = Just (SyntheticEvent a)
castSyntheticEvent _ | otherwise = Nothing

-- | Using the NFData idea from React/Flux/PropertiesAndEvents.hs
-- React re-uses SyntheticEvent from a pool, which means it may no longer be valid if we lazily
-- parse it. However, we still want lazy parsing so we don't parse unnecessary fields.
-- This safe interface requires two input functions:
-- 1. a function to reduce SyntheticEvent to a NFData. The mkEventCallback will ensure that the
-- NFData is forced which will ensure all the required fields from Synthetic event has been parsed.
-- This function must not block.
-- 2. a second function that uses the NFData. This function may block.
-- mkEventHandler results in a function that you can safely pass into 'GHC.Foreign.Callback.syncCallback1'
-- with 'GHCJS.Foreign.Callback.ContinueAsync'.
-- NB. Since Javascript is single threaded, and Haskell is lazy, GHCJS threads are a strange
-- mixture of synchronous and asynchronous threads, where a synchronous thread might be converted
-- to an asynchronous thread if a "black hole" is encountered.
-- See https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Concurrent.hs
mkEventHandler :: NFData a => (evt -> a) -> (a -> b) -> (evt -> b)
mkEventHandler f g evt = g $!! f evt

foreign import javascript unsafe
    "$1.preventDefault()"
    js_preventDefault :: SyntheticEvent -> IO ()

foreign import javascript unsafe
    "$1.isDefaultPrevented()"
    js_isDefaultPrevented :: SyntheticEvent -> Bool

foreign import javascript unsafe
    "$1.stopPropagation()"
    js_stopPropagation :: SyntheticEvent -> IO ()

foreign import javascript unsafe
    "$1.isPropagationStopped()"
    js_isPropagationStopped :: SyntheticEvent -> Bool

-- | Every `SyntheticEvent` can be parsed to an `Event`.
data Event = Event
    { eventBubbles :: Bool
    , eventCancelable :: Bool
    , eventCurrentTarget :: DOMEventTarget
    , eventDefaultPrevented :: Bool
    , eventEventPhase :: Int
    , eventIsTrusted :: Bool
    , eventNativeEvent :: DOMEvent
    , eventTarget :: DOMEventTarget
    , eventTimestamp :: Int
    -- type is a reserved word, so prefix to eventType
    , eventEventType :: T.Text
    }

-- makeFields ''Event

-- | A pure version of 'GHCJS.Foreign.Internal.unsafeProperty with the arguments flipped.
-- since it's already marked unsafe, we might as well lie about possible side effects too.
foreign import javascript unsafe "$1[$2]"
  js_unsafeProperty :: JSVal -> JSString -> JSVal

unsafeProperty :: PFromJSVal a => JSVal -> JSString -> a
unsafeProperty v = pFromJSVal . js_unsafeProperty v

-- | This should alway be safe to use (if given a valid SyntheticEvent).
parseEvent :: SyntheticEvent -> Event
parseEvent (SyntheticEvent evt) =
    Event
    { eventBubbles = unsafeProperty evt "bubbles"
    , eventCancelable = unsafeProperty evt "cancelable"
    , eventCurrentTarget = DOMEventTarget $ js_unsafeProperty evt "currentTarget"
    , eventDefaultPrevented = unsafeProperty evt "defaultPrevented"
    , eventEventPhase = unsafeProperty evt "eventPhase"
    , eventIsTrusted = unsafeProperty evt "isTrusted"
    , eventNativeEvent = DOMEvent $ js_unsafeProperty evt "nativeEvent"
    , eventTarget = DOMEventTarget $ js_unsafeProperty evt "target"
    , eventTimestamp = unsafeProperty evt "timestamp"
    , eventEventType = unsafeProperty evt "type"
    }

-- | See https://www.w3.org/TR/DOM-Level-3-Events-key/#keys-modifier
data ModifierKey
    = Alt
    | AltGraph
    | CapsLock
    | Control
    | Fn
    | FnLock
    | Hyper
    | Meta
    | NumLock
    | ScrollLock
    | Shift
    | Super
    | Symbol
    | SymbolLock
    deriving (Show)

-- | Mouse and Drag/Drop events
-- https://facebook.github.io/react/docs/events.html#mouse-events
-- https://developer.mozilla.org/en-US/docs/Web/Events
-- onClick (click) onContextMenu (contextmenu) onDoubleClick (dblclick)
-- onDrag (drag) onDragEnd (dragend) onDragEnter (dragenter) onDragExit (dragexit)
-- onDragLeave (dragleave) onDragOver (dragover) onDragStart (dragstart)
-- onDrop (drop) onMouseDown (mousedown) onMouseEnter (mouseenter) onMouseLeave (mouseleave)
-- onMouseMove (mousemove) onMouseOut (mouseout) onMouseOver (mouseover) onMouseUp (mouseup)
data MouseEvent = MouseEvent
  { mouseEventAltKey :: Bool
  , mouseEventButton :: Int
  , mouseEventButtons :: Int
  , mouseEventClientX :: Int
  , mouseEventClientY :: Int
  , mouseEventCtrlKey :: Bool
  , mouseEventGetModifierState :: ModifierKey -> Bool
  , mouseEventMetaKey :: Bool
  , mouseEventPageX :: Int
  , mouseEventPageY :: Int
  , mouseEventRelatedTarget :: DOMEventTarget
  , mouseEventScreenX :: Int
  , mouseEventScreenY :: Int
  , mouseEventShiftKey :: Bool
  }

foreign import javascript unsafe
    "$1.getModifierState($2)"
    js_unsafeGetModifierState :: JSVal -> JSString -> JSVal

unsafeGetModifierState :: JSVal -> ModifierKey -> Bool
unsafeGetModifierState obj = fromJSBool . js_unsafeGetModifierState obj . pack . show

foreign import javascript unsafe
    "($1 instanceof MouseEvent)"
    js_isMouseEvent :: JSVal -> Bool

parseMouseEvent :: SyntheticEvent -> Maybe MouseEvent
parseMouseEvent (SyntheticEvent evt) | js_isMouseEvent (js_unsafeProperty evt "nativeEvent") = Just $
    MouseEvent
    { mouseEventAltKey = unsafeProperty evt "altKey"
    , mouseEventButton = unsafeProperty evt "button"
    , mouseEventButtons = unsafeProperty evt "buttons"
    , mouseEventClientX = unsafeProperty evt "clientX"
    , mouseEventClientY = unsafeProperty evt "clientY"
    , mouseEventCtrlKey = unsafeProperty evt "ctrlKey"
    , mouseEventGetModifierState = unsafeGetModifierState evt
    , mouseEventMetaKey = unsafeProperty evt "metaKey"
    , mouseEventPageX = unsafeProperty evt "pageX"
    , mouseEventPageY = unsafeProperty evt "pageY"
    , mouseEventRelatedTarget = DOMEventTarget $ js_unsafeProperty evt "relatedTarget"
    , mouseEventScreenX = unsafeProperty evt "screenX"
    , mouseEventScreenY = unsafeProperty evt "xcreenY"
    , mouseEventShiftKey = unsafeProperty evt "shiftKey"
    }
parseMouseEvent _ | otherwise = Nothing
