{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | This module based on React/Flux/PropertiesAndEvents.hs.
module Glazier.React.Event
  ( DOMEventTarget
  , DOMEvent
  , SyntheticEvent
  , castSyntheticEvent
  , eventHandler
  , eventHandlerM
  , Event(..)
  , preventDefault
  , isDefaultPrevented
  , stopPropagation
  , isPropagationStopped
  , fromSyntheticEvent
  , parseMaybeEvent
  , MouseEvent(..)
  , parseMouseEvent
  , KeyboardEvent(..)
  , parseKeyboardEvent
  )
where

import Control.DeepSeq
import qualified Data.JSString as J
import qualified GHCJS.Foreign as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Cast as J

-- | The object that dispatched the event.
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
newtype DOMEventTarget = DOMEventTarget J.JSVal

instance J.IsJSVal DOMEventTarget
instance J.PToJSVal DOMEventTarget where
    pToJSVal = J.jsval

instance J.Cast DOMEventTarget where
    unsafeWrap = DOMEventTarget
    instanceRef _ = js_DOMEventTarget

foreign import javascript unsafe
    "EventTarget"
    js_DOMEventTarget :: J.JSVal

-- | The native event
-- https://developer.mozilla.org/en-US/docs/Web/API/Event
newtype DOMEvent = DOMEvent J.JSVal

instance J.IsJSVal DOMEvent
instance J.PToJSVal DOMEvent where
    pToJSVal = J.jsval

instance J.Cast DOMEvent where
    unsafeWrap = DOMEvent
    instanceRef _ = js_DOMEvent

foreign import javascript unsafe
    "Event"
    js_DOMEvent :: J.JSVal

-- | Every event in React is a synthetic event, a cross-browser wrapper around the native event.
-- 'SyntheticEvent' must only be used in the first part of 'eventHandler'.
newtype SyntheticEvent = SyntheticEvent J.JSVal

instance J.IsJSVal SyntheticEvent
instance J.PToJSVal SyntheticEvent where
    pToJSVal = J.jsval

foreign import javascript unsafe
    "($1 && $1.nativeEvent && $1.nativeEvent instanceof Event)"
    js_isSyntheticEvent :: J.JSVal -> Bool

-- | SyntheticEvent cannot be a Javascript.Cast
-- See https://github.com/ghcjs/ghcjs-base/issues/86
castSyntheticEvent :: J.JSVal -> Maybe SyntheticEvent
castSyntheticEvent a | js_isSyntheticEvent a = Just (SyntheticEvent a)
castSyntheticEvent _ | otherwise = Nothing

-- | Using the NFData idea from React/Flux/PropertiesAndEvents.hs
-- React re-uses SyntheticEvent from a pool, which means it may no longer be valid if we lazily
-- parse it. However, we still want lazy parsing so we don't parse unnecessary fields.
-- This safe interface requires two input functions:
-- 1. a function to reduce SyntheticEvent to a NFData. The mkEventCallback will ensure that the
-- NFData is forced which will ensure all the required fields from Synthetic event has been parsed.
-- This function must not block.
-- 2. a second function that uses the NFData. This function is allowed to block.
-- mkEventHandler results in a function that you can safely pass into 'GHC.Foreign.Callback.syncCallback1'
-- with 'GHCJS.Foreign.Callback.ContinueAsync'.
-- The reason of this is because Javascript is single threaded, but Haskell is lazy.
-- Therefore GHCJS threads are a strange mixture of synchronous and asynchronous threads,
-- where a synchronous thread might be converted to an asynchronous thread if a "black hole" is encountered.
-- See https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Concurrent.hs
eventHandler :: NFData a => (evt -> a) -> (a -> b) -> (evt -> b)
eventHandler goStrict goLazy evt = goLazy $!! goStrict evt

-- | a monadic version of eventHandler
-- The monad's effects must not block!
eventHandlerM :: (Monad m, NFData a) => (evt -> m a) -> (a -> m b) -> (evt -> m b)
eventHandlerM goStrict goLazy evt = do
    r <- goStrict evt
    goLazy $!! r

foreign import javascript unsafe
    "$1.preventDefault()"
    js_preventDefault :: SyntheticEvent -> IO ()

preventDefault :: SyntheticEvent -> IO ()
preventDefault = js_preventDefault

foreign import javascript unsafe
    "$1.isDefaultPrevented()"
    js_isDefaultPrevented :: SyntheticEvent -> Bool

isDefaultPrevented :: SyntheticEvent -> Bool
isDefaultPrevented = js_isDefaultPrevented

foreign import javascript unsafe
    "$1.stopPropagation()"
    js_stopPropagation :: SyntheticEvent -> IO ()

stopPropagation :: SyntheticEvent -> IO ()
stopPropagation = js_stopPropagation

foreign import javascript unsafe
    "$1.isPropagationStopped()"
    js_isPropagationStopped :: SyntheticEvent -> Bool

isPropagationStopped :: SyntheticEvent -> Bool
isPropagationStopped = js_isPropagationStopped

-- | Every `SyntheticEvent` can be parsed to an `Event`.
-- 'Event' must only be used in the first part of 'eventHandler'.
data Event = Event
    { bubbles :: Bool
    , cancelable :: Bool
    , currentTarget :: DOMEventTarget
    , defaultPrevented :: Bool
    , eventPhase :: Int
    , isTrusted :: Bool
    , nativeEvent :: DOMEvent
    , target :: DOMEventTarget
    , timeStamp :: Int
    -- type is a reserved word, so prefix to eventType
    , eventType :: J.JSString
    }

-- | unsafe and non-IO to enable lazy parsing. See mkEventHandler
foreign import javascript unsafe "$1[$2]"
  js_unsafeProperty :: J.JSVal -> J.JSString -> J.JSVal

unsafeProperty :: J.PFromJSVal a => J.JSVal -> J.JSString -> a
unsafeProperty v = J.pFromJSVal . js_unsafeProperty v

-- | This should alway be safe to use
fromSyntheticEvent :: SyntheticEvent -> Event
fromSyntheticEvent (SyntheticEvent evt) = doParseEvent evt

-- | This is for parsing DOM events not from the React framework (eg hashchange)
-- Will return Nothing if J.JSVal is null or undefined
parseMaybeEvent :: J.JSVal -> Maybe Event
parseMaybeEvent evt = if J.isUndefined evt || J.isNull evt
    then Nothing
    else Just $ doParseEvent evt

doParseEvent :: J.JSVal -> Event
doParseEvent evt =
    Event
    { bubbles = unsafeProperty evt "bubbles"
    , cancelable = unsafeProperty evt "cancelable"
    , currentTarget = DOMEventTarget $ js_unsafeProperty evt "currentTarget"
    , defaultPrevented = unsafeProperty evt "defaultPrevented"
    , eventPhase = unsafeProperty evt "eventPhase"
    , isTrusted = unsafeProperty evt "isTrusted"
    , nativeEvent = DOMEvent evt
    , target = DOMEventTarget $ js_unsafeProperty evt "target"
    , timeStamp = unsafeProperty evt "timeStamp"
    , eventType = unsafeProperty evt "type"
    }


-- | Mouse and Drag/Drop events
-- 'MouseEvent' must only be used in the first part of 'eventHandler'.
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
  , relatedTarget :: DOMEventTarget
  , screenX :: Int
  , screenY :: Int
  , shiftKey :: Bool
  }

-- | unsafe to enable lazy parsing. See mkEventHandler
foreign import javascript unsafe
    "$1.getModifierState($2)"
    js_unsafeGetModifierState :: J.JSVal -> J.JSString -> J.JSVal

-- | See https://www.w3.org/TR/DOM-Level-3-Events-key/#keys-modifier
unsafeGetModifierState :: J.JSVal -> J.JSString -> Bool
unsafeGetModifierState obj k = J.fromJSBool $ js_unsafeGetModifierState obj k

foreign import javascript unsafe
    "($1 instanceof MouseEvent)"
    js_isMouseEvent :: J.JSVal -> Bool

parseMouseEvent :: SyntheticEvent -> Maybe MouseEvent
parseMouseEvent (SyntheticEvent evt) | js_isMouseEvent (js_unsafeProperty evt "nativeEvent") = Just $
    MouseEvent
    { altKey = unsafeProperty evt "altKey"
    , button = unsafeProperty evt "button"
    , buttons = unsafeProperty evt "buttons"
    , clientX = unsafeProperty evt "clientX"
    , clientY = unsafeProperty evt "clientY"
    , ctrlKey = unsafeProperty evt "ctrlKey"
    , getModifierState = unsafeGetModifierState evt
    , metaKey = unsafeProperty evt "metaKey"
    , pageX = unsafeProperty evt "pageX"
    , pageY = unsafeProperty evt "pageY"
    , relatedTarget = DOMEventTarget $ js_unsafeProperty evt "relatedTarget"
    , screenX = unsafeProperty evt "screenX"
    , screenY = unsafeProperty evt "xcreenY"
    , shiftKey = unsafeProperty evt "shiftKey"
    }
parseMouseEvent _ | otherwise = Nothing

-- | Keyboard events
-- 'KeyboardEvent' must only be used in the first part of 'eventHandler'.
-- https://facebook.github.io/react/docs/events.html#keyboard-events
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

foreign import javascript unsafe
    "($1 instanceof KeyboardEvent)"
    js_isKeyboardEvent :: J.JSVal -> Bool

parseKeyboardEvent :: SyntheticEvent -> Maybe KeyboardEvent
parseKeyboardEvent (SyntheticEvent evt) | js_isKeyboardEvent (js_unsafeProperty evt "nativeEvent") = Just $
    KeyboardEvent
    { altKey = unsafeProperty evt "altKey"
    , charCode = unsafeProperty evt "charCode"
    , ctrlKey = unsafeProperty evt "ctrlKey"
    , getModifierState = unsafeGetModifierState evt
    , key = unsafeProperty evt "key"
    , keyCode = unsafeProperty evt "keyCode"
    , locale = unsafeProperty evt "locale"
    , location = unsafeProperty evt "location"
    , metaKey = unsafeProperty evt "metaKey"
    , repeat = unsafeProperty evt "repeat"
    , shiftkey = unsafeProperty evt "shiftkey"
    , which = unsafeProperty evt "which"
    }
parseKeyboardEvent _ | otherwise = Nothing
