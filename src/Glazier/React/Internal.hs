{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Internal where

import GHCJS.Types (IsJSVal, jsval)
import GHCJS.Marshal.Pure (PToJSVal(..))

-- | This is to enable conversion between equivalent classes 'IsJSVal' and 'PToJSVal'
-- Eg. You can convert ((PureJSVal Object) to JSVal with pToJSVal without requiring IO)
newtype PureJSVal a = PureJSVal a
    deriving IsJSVal

instance IsJSVal a => PToJSVal (PureJSVal a) where
    pToJSVal (PureJSVal a) = jsval a
