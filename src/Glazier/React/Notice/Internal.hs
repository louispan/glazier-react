{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Notice.Internal
  ( Notice(..)  -- constructor is exported
  , preventDefault
  , isDefaultPrevented
  , stopPropagation
  , isPropagationStopped
  , unsafeGetProperty
  , unsafeGetModifierState
  )
where

import qualified GHC.Generics as G
import qualified GHCJS.Foreign as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | Every event in React is a synthetic event, a cross-browser wrapper around the native event.
-- which reused from a pool.
-- So it is dangerous to keep a reference to a 'Notice' since it may expire and contain
-- other things without you knowing.
-- All relevant data from the 'Notice' must be consumed as soon you get one.
-- That is, 'Notice' must only be used in the first part of 'handleEvent'.
-- It is not an instance of NFData and so cannot be returned into the second lazy part of 'handleEvent'
newtype Notice = Notice JE.JSRep -- not J.JSVal so the show instance is more useful
    deriving (G.Generic, Show)

instance JE.FromJS Notice where
    fromJS a | js_isNotice a = Just $ Notice $ JE.JSRep a
    fromJS _ = Nothing

preventDefault :: Notice -> IO ()
preventDefault = js_preventDefault

isDefaultPrevented :: Notice -> Bool
isDefaultPrevented = js_isDefaultPrevented

stopPropagation :: Notice -> IO ()
stopPropagation = js_stopPropagation

isPropagationStopped :: Notice -> Bool
isPropagationStopped = js_isPropagationStopped

-- | Within the strict part of 'handleEventM'
-- the Notice is effectively immutable.
-- We want to maintain this lie so that we can lazily parse only the
-- properties the event handler is interested in.
-- This will throw if J.JSVal is null, or not convertible to the desired type
-- so we are assuming that Notice will behave nicely.
unsafeGetProperty :: J.PFromJSVal a => J.JSVal -> J.JSString -> a
unsafeGetProperty v = J.pFromJSVal . js_unsafeGetProperty v

-- | See https://www.w3.org/TR/DOM-Level-3-Events-key/#keys-modifier
-- This will throw if J.JSVal is null, but shouldn't happen since we've
-- already check for a valid Notice
unsafeGetModifierState :: J.JSVal -> J.JSString -> Bool
unsafeGetModifierState obj k = J.fromJSBool $ js_unsafeGetModifierState obj k

#ifdef __GHCJS__

foreign import javascript unsafe
    "($1 && $1['nativeEvent'] && $1['nativeEvent'] instanceof Event)"
    js_isNotice :: J.JSVal -> Bool

foreign import javascript unsafe
    "$1['preventDefault']()"
    js_preventDefault :: Notice -> IO ()

foreign import javascript unsafe
    "$1['isDefaultPrevented']()"
    js_isDefaultPrevented :: Notice -> Bool

foreign import javascript unsafe
    "$1['stopPropagation']()"
    js_stopPropagation :: Notice -> IO ()

foreign import javascript unsafe
    "$1['isPropagationStopped']()"
    js_isPropagationStopped :: Notice -> Bool

-- | unsafe and non-IO to enable lazy parsing. See handleEvent
foreign import javascript unsafe "$1[$2]"
  js_unsafeGetProperty :: J.JSVal -> J.JSString -> J.JSVal

-- | unsafe to enable lazy parsing. See handleEvent
foreign import javascript unsafe
    "$1['getModifierState']($2)"
    js_unsafeGetModifierState :: J.JSVal -> J.JSString -> J.JSVal

#else

js_isNotice :: J.JSVal -> Bool
js_isNotice _ = False

js_preventDefault :: Notice -> IO ()
js_preventDefault _ = pure ()

js_isDefaultPrevented :: Notice -> Bool
js_isDefaultPrevented _ = False

js_stopPropagation :: Notice -> IO ()
js_stopPropagation _ = pure ()

js_isPropagationStopped :: Notice -> Bool
js_isPropagationStopped _ = False

-- | unsafe and non-IO to enable lazy parsing. See handleEvent
js_unsafeGetProperty :: J.JSVal -> J.JSString -> J.JSVal
js_unsafeGetProperty _ _ = J.nullRef

-- | unsafe to enable lazy parsing. See handleEvent
js_unsafeGetModifierState :: J.JSVal -> J.JSString -> J.JSVal
js_unsafeGetModifierState _ _ = J.nullRef

#endif
