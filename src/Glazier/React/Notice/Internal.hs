{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.DOM.Event.Notice.Internal
  ( Notice(..) -- ^ constructor is exported
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
newtype Notice = Notice J.JSVal
    deriving (G.Generic, Show)

instance JE.FromJS Notice where
    fromJS a | js_isNotice a = Just $ Notice a
    fromJS _ = Nothing

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
    "$1 && $1['nativeEvent'] && $1['nativeEvent'] instanceof Event"
    js_isNotice :: J.JSVal -> Bool

-- | unsafe and non-IO to enable lazy parsing. See handleEvent
foreign import javascript unsafe "$1[$2]"
    js_unsafeGetProperty :: J.JSVal -> J.JSString -> J.JSVal

-- | unsafe and non-IO to enable lazy parsing. See handleEvent
foreign import javascript unsafe
    "$1['getModifierState']($2)"
    js_unsafeGetModifierState :: J.JSVal -> J.JSString -> J.JSVal

#else

js_isNotice :: J.JSVal -> Bool
js_isNotice _ = False

-- | unsafe and non-IO to enable lazy parsing.
js_unsafeGetProperty :: J.JSVal -> J.JSString -> J.JSVal
js_unsafeGetProperty _ _ = J.nullRef

-- | unsafe to enable lazy parsing
js_unsafeGetModifierState :: J.JSVal -> J.JSString -> J.JSVal
js_unsafeGetModifierState _ _ = J.nullRef

#endif
