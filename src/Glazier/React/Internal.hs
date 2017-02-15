{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Internal where

import GHCJS.Types (IsJSVal, jsval)
import GHCJS.Marshal.Pure (PToJSVal(..))

-- | GHCJ base is still immature
-- This is to enable conversion between equivalent classes 'IsJSVal' and 'PToJSVal'
newtype PureJSVal a = PureJSVal a
    deriving IsJSVal

instance IsJSVal a => PToJSVal (PureJSVal a) where
    pToJSVal (PureJSVal a) = jsval a
