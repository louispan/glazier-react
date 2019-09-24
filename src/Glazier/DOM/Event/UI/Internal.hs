{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.DOM.Event.UI.Internal
( NativeUIEvent(..)
, SyntheticUIEvent(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.DOM.Event
import qualified JavaScript.Extras as JE

newtype NativeUIEvent = NativeUIEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

newtype SyntheticUIEvent = SyntheticUIEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString)

instance JE.FromJS NativeUIEvent where
    validInstance = js_isNativeUIEvent
    fromJS a | js_isNativeUIEvent a = Just $ NativeUIEvent a
    fromJS _ = Nothing

instance JE.FromJS SyntheticUIEvent where
    validInstance = js_isSyntheticUIEvent
    fromJS a | js_isSyntheticUIEvent a = Just $ SyntheticUIEvent a
    fromJS _ = Nothing

instance IEvent NativeUIEvent

instance IEvent SyntheticUIEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof UIEvent"
    js_isNativeUIEvent :: J.JSVal -> Bool

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Object && $1['nativeEvent'] && $1['nativeEvent'] instanceof UIEvent"
    js_isSyntheticUIEvent :: J.JSVal -> Bool

#else

js_isNativeUIEvent :: J.JSVal -> Bool
js_isNativeUIEvent _ = False


js_isSyntheticUIEvent :: J.JSVal -> Bool
js_isSyntheticUIEvent _ = False

#endif
