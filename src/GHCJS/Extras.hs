{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHCJS.Extras
    ( strval
    , getProperty
    , classNames
    , PureJSVal(..)
    ) where

import qualified GHCJS.Types as J
import qualified Data.JSString as J
import qualified GHCJS.Marshal.Pure as J

-- | This makes it easier to use OverloadedStrings with inputs that accept a JSVal that could be a JSString
strval :: J.JSString -> J.JSVal
strval = J.jsval

-- | This can throw an exception if $2 is null
foreign import javascript unsafe "$2[$1]"
  js_unsafeGetProp :: J.JSString -> J.JSVal -> IO J.JSVal

-- | get a property of any JSVal. If a null or undefined is queried, the result will also be null
getProperty :: J.JSString -> J.JSVal -> IO J.JSVal
getProperty _ x | J.isUndefined x || J.isNull x = pure x
getProperty p x = js_unsafeGetProp p x

classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = J.jsval . J.unwords . fmap fst . filter snd

-- | This is to enable conversion between equivalent classes 'IsJSVal' and 'PToJSVal'
-- Eg. You can convert ((PureJSVal Object) to JSVal with pToJSVal without requiring IO)
newtype PureJSVal a = PureJSVal a
    deriving J.IsJSVal

instance J.IsJSVal a => J.PToJSVal (PureJSVal a) where
    pToJSVal (PureJSVal a) = J.jsval a
