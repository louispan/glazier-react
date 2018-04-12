{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module based on React/Flux/PropertiesAndEvents.hs.
module Glazier.React.NativeEvent.Internal
  ( NativeEvent(..)
  , )
where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | The native event
-- https://developer.mozilla.org/en-US/docs/Web/API/Event
newtype NativeEvent =
    NativeEvent JE.JSRep -- not J.JSVal so the show instance is more useful
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS NativeEvent where
    fromJS a | js_isNativeEvent a = Just $ NativeEvent $ JE.JSRep a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 instanceof Event"
    js_isNativeEvent :: J.JSVal -> Bool

#else

js_isNativeEvent :: J.JSVal -> Bool
js_isNativeEvent _ = False

#endif
