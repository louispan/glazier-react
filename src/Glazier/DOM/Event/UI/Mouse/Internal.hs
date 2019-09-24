{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.DOM.Event.UI.Mouse.Internal
( NativeMouseEvent(..)
, SyntheticMouseEvent(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.DOM.Event
import Glazier.DOM.Event.UI
import qualified JavaScript.Extras as JE

newtype NativeMouseEvent = NativeMouseEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

newtype SyntheticMouseEvent = SyntheticMouseEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString)

instance JE.FromJS NativeMouseEvent where
    validInstance = js_isNativeMouseEvent
    fromJS a | js_isNativeMouseEvent a = Just $ NativeMouseEvent a
    fromJS _ = Nothing

instance JE.FromJS SyntheticMouseEvent where
    validInstance = js_isSyntheticMouseEvent
    fromJS a | js_isSyntheticMouseEvent a = Just $ SyntheticMouseEvent a
    fromJS _ = Nothing

instance IEvent NativeMouseEvent
instance IUIEvent NativeMouseEvent

instance IEvent SyntheticMouseEvent
instance IUIEvent SyntheticMouseEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof MouseEvent"
    js_isNativeMouseEvent :: J.JSVal -> Bool

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Object && $1['nativeEvent'] && $1['nativeEvent'] instanceof MouseEvent"
    js_isSyntheticMouseEvent :: J.JSVal -> Bool

#else

js_isNativeMouseEvent :: J.JSVal -> Bool
js_isNativeMouseEvent _ = False

js_isSyntheticMouseEvent :: J.JSVal -> Bool
js_isSyntheticMouseEvent _ = False

#endif
