{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module based on React/Flux/PropertiesAndEvents.hs.
module Glazier.React.Event
  ( EventTarget
  , NativeEvent
  , SyntheticEvent
  , handleEvent
  , handleEventM
  , Event(..)
  , preventDefault
  , isDefaultPrevented
  , stopPropagation
  , isPropagationStopped
  , parseEvent
  , MouseEvent(..)
  , parseMouseEvent
  , KeyboardEvent(..)
  , parseKeyboardEvent
  )
where

import Control.DeepSeq
import qualified Data.JSString as J
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Foreign as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | The object that dispatched the event.
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
newtype EventTarget =
    EventTarget JE.JSVar
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS EventTarget where
    fromJS a | js_isEventTarget a = Just $ EventTarget $ JE.JSVar a
    fromJS _ = Nothing

-- | The native event
-- https://developer.mozilla.org/en-US/docs/Web/API/Event
newtype NativeEvent =
    NativeEvent JE.JSVar
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS NativeEvent where
    fromJS a | js_isNativeEvent a = Just $ NativeEvent $ JE.JSVar a
    fromJS _ = Nothing

-- | Every event in React is a synthetic event, a cross-browser wrapper around the native event.
-- 'SyntheticEvent' must only be used in the first part of 'handleEvent'.
-- It is not an instance of NFData and so cannot be returned into the second lazy part of 'handleEvent'
newtype SyntheticEvent = SyntheticEvent J.JSVal
    deriving (G.Generic)

instance JE.FromJS SyntheticEvent where
    fromJS a | js_isSyntheticEvent a = Just $ SyntheticEvent a
    fromJS _ = Nothing

-- | Using the NFData idea from React/Flux/PropertiesAndEvents.hs
-- React re-uses SyntheticEvent from a pool, which means it may no longer be valid if we lazily
-- parse it. However, we still want lazy parsing so we don't parse unnecessary fields.
-- Additionally, we don't want to block during the event handling.The reason this is a problem is
-- because Javascript is single threaded, but Haskell is lazy.
-- Therefore GHCJS threads are a strange mixture of synchronous and asynchronous threads,
-- where a synchronous thread might be converted to an asynchronous thread if a "black hole" is encountered.
-- See https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Concurrent.hs
-- This safe interface requires two input functions:
-- 1. a function to reduce SyntheticEvent to a NFData. The handleEvent will ensure that the
-- NFData is forced which will ensure all the required fields from Synthetic event has been parsed.
-- This function must not block.
-- 2. a second function that uses the NFData. This function is allowed to block.
-- handleEvent results in a function that you can safely pass into 'GHC.Foreign.Callback.syncCallback1'
-- with 'GHCJS.Foreign.Callback.ContinueAsync'.
handleEvent :: NFData a => (evt -> a) -> (a -> b) -> (evt -> b)
handleEvent goStrict goLazy evt = goLazy $!! goStrict evt

-- | a monadic version of handleEvent
-- The monad's effects must not block!
handleEventM :: (Monad m, NFData a) => (evt -> m a) -> (a -> m b) -> (evt -> m b)
handleEventM goStrict goLazy evt = do
    r <- goStrict evt
    goLazy $!! r

preventDefault :: SyntheticEvent -> IO ()
preventDefault = js_preventDefault

isDefaultPrevented :: SyntheticEvent -> Bool
isDefaultPrevented = js_isDefaultPrevented

stopPropagation :: SyntheticEvent -> IO ()
stopPropagation = js_stopPropagation

isPropagationStopped :: SyntheticEvent -> Bool
isPropagationStopped = js_isPropagationStopped

-- | Every `SyntheticEvent` can be parsed to an `Event`.
-- 'Event' must only be used in the first part of 'handleEvent'.
data Event = Event
    { bubbles :: Bool
    , cancelable :: Bool
    , currentTarget :: EventTarget
    , defaultPrevented :: Bool
    , eventPhase :: Int
    , isTrusted :: Bool
    , nativeEvent :: NativeEvent
    , target :: EventTarget
    , timeStamp :: Int
    -- type is a reserved word, so prefix to eventType
    , eventType :: J.JSString
    }
    deriving (G.Generic)
instance NFData Event

-- | We can lie about this not being in IO because
-- within the strict part of 'handleEventM'
-- the SyntheticEvent is effectively immutable.
-- In reality SyntheticEvent is reused from a pool.
-- We want to maintain this lie so that we can lazily parse only the
-- properties the event handler is interested in.
-- This will throw if J.JSVal is null, or not convertible to the desired type
-- so we are assuming that SyntheticEvent will behave nicely.
unsafeProperty :: J.PFromJSVal a => J.JSVal -> J.JSString -> a
unsafeProperty v = J.pFromJSVal . js_unsafeProperty v

-- | We can lie about this not being in IO because
-- within the strict part of 'handleEventM'
-- the SyntheticEvent is effectively immutable.
parseEvent :: SyntheticEvent -> Event
parseEvent (SyntheticEvent evt) =
    Event
    { bubbles = unsafeProperty evt "bubbles"
    , cancelable = unsafeProperty evt "cancelable"
    , currentTarget = EventTarget $ JE.JSVar $ js_unsafeProperty evt "currentTarget"
    , defaultPrevented = unsafeProperty evt "defaultPrevented"
    , eventPhase = unsafeProperty evt "eventPhase"
    , isTrusted = unsafeProperty evt "isTrusted"
    , nativeEvent = NativeEvent $ JE.JSVar $ evt
    , target = EventTarget $ JE.JSVar $ js_unsafeProperty evt "target"
    , timeStamp = unsafeProperty evt "timeStamp"
    , eventType = unsafeProperty evt "type"
    }

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

-- | See https://www.w3.org/TR/DOM-Level-3-Events-key/#keys-modifier
-- This will throw if J.JSVal is null, but shouldn't happen since we've
-- already check for a valid SyntheticEvent
unsafeGetModifierState :: J.JSVal -> J.JSString -> Bool
unsafeGetModifierState obj k = J.fromJSBool $ js_unsafeGetModifierState obj k

-- | We can lie about this not being in IO because
-- within the strict part of 'handleEventM'
-- the SyntheticEvent is effectively immutable.
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
    , relatedTarget = EventTarget $ JE.JSVar $ js_unsafeProperty evt "relatedTarget"
    , screenX = unsafeProperty evt "screenX"
    , screenY = unsafeProperty evt "xcreenY"
    , shiftKey = unsafeProperty evt "shiftKey"
    }
parseMouseEvent _ | otherwise = Nothing

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
-- the SyntheticEvent is effectively immutable.
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

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 instanceof EventTarget"
    js_isEventTarget :: J.JSVal -> Bool

foreign import javascript unsafe
    "$1 instanceof Event"
    js_isNativeEvent :: J.JSVal -> Bool

foreign import javascript unsafe
    "($1 && $1['nativeEvent'] && $1['nativeEvent'] instanceof Event)"
    js_isSyntheticEvent :: J.JSVal -> Bool

foreign import javascript unsafe
    "$1['preventDefault']()"
    js_preventDefault :: SyntheticEvent -> IO ()

foreign import javascript unsafe
    "$1['isDefaultPrevented']()"
    js_isDefaultPrevented :: SyntheticEvent -> Bool

foreign import javascript unsafe
    "$1['stopPropagation']()"
    js_stopPropagation :: SyntheticEvent -> IO ()

foreign import javascript unsafe
    "$1['isPropagationStopped']()"
    js_isPropagationStopped :: SyntheticEvent -> Bool

-- | unsafe and non-IO to enable lazy parsing. See handleEvent
foreign import javascript unsafe "$1[$2]"
  js_unsafeProperty :: J.JSVal -> J.JSString -> J.JSVal

-- | unsafe to enable lazy parsing. See handleEvent
foreign import javascript unsafe
    "$1['getModifierState']($2)"
    js_unsafeGetModifierState :: J.JSVal -> J.JSString -> J.JSVal

foreign import javascript unsafe
    "($1 instanceof MouseEvent)"
    js_isMouseEvent :: J.JSVal -> Bool

foreign import javascript unsafe
    "($1 instanceof KeyboardEvent)"
    js_isKeyboardEvent :: J.JSVal -> Bool

#else

js_isEventTarget :: J.JSVal -> Bool
js_isEventTarget _ = False

js_isNativeEvent :: J.JSVal -> Bool
js_isNativeEvent _ = False

js_isSyntheticEvent :: J.JSVal -> Bool
js_isSyntheticEvent _ = False

js_preventDefault :: SyntheticEvent -> IO ()
js_preventDefault _ = pure ()

js_isDefaultPrevented :: SyntheticEvent -> Bool
js_isDefaultPrevented _ = False

js_stopPropagation :: SyntheticEvent -> IO ()
js_stopPropagation _ = pure ()

js_isPropagationStopped :: SyntheticEvent -> Bool
js_isPropagationStopped _ = False

-- | unsafe and non-IO to enable lazy parsing. See handleEvent
js_unsafeProperty :: J.JSVal -> J.JSString -> J.JSVal
js_unsafeProperty _ _ = J.nullRef

-- | unsafe to enable lazy parsing. See handleEvent
js_unsafeGetModifierState :: J.JSVal -> J.JSString -> J.JSVal
js_unsafeGetModifierState _ _ = J.nullRef

js_isMouseEvent :: J.JSVal -> Bool
js_isMouseEvent _ = False

js_isKeyboardEvent :: J.JSVal -> Bool
js_isKeyboardEvent _ = False

#endif
