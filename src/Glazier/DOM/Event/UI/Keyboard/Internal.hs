{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.DOM.Event.UI.Keyboard.Internal
( NativeKeyboardEvent(..)
, SyntheticKeyboardEvent(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.DOM.Event
import Glazier.DOM.Event.UI
import qualified JavaScript.Extras as JE

newtype NativeKeyboardEvent = NativeKeyboardEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

newtype SyntheticKeyboardEvent = SyntheticKeyboardEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString)

instance JE.FromJS NativeKeyboardEvent where
    validFromJS = js_isNativeKeyboardEvent
    fromJS a | js_isNativeKeyboardEvent a = Just $ NativeKeyboardEvent a
    fromJS _ = Nothing

instance JE.FromJS SyntheticKeyboardEvent where
    validFromJS = js_isSyntheticKeyboardEvent
    fromJS a | js_isSyntheticKeyboardEvent a = Just $ SyntheticKeyboardEvent a
    fromJS _ = Nothing

instance IEvent NativeKeyboardEvent
instance IUIEvent NativeKeyboardEvent

instance IEvent SyntheticKeyboardEvent
instance IUIEvent SyntheticKeyboardEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof KeyboardEvent"
    js_isNativeKeyboardEvent :: J.JSVal -> Bool

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Object && $1['nativeEvent'] && $1['nativeEvent'] instanceof KeyboardEvent"
    js_isSyntheticKeyboardEvent :: J.JSVal -> Bool

#else

js_isNativeKeyboardEvent :: J.JSVal -> Bool
js_isNativeKeyboardEvent _ = False

js_isSyntheticKeyboardEvent :: J.JSVal -> Bool
js_isSyntheticKeyboardEvent _ = False

#endif
