{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHCJS.Extras
    ( strval
    , getProperty
    , classNames
    , PureJSVal(..)
    , Property
    , toMaybeJSObject
    , toJSObject
    ) where

import Data.Foldable
import qualified Data.JSString as J
import qualified Data.Text as T
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Object as JO

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

-- | This is to enable conversion between equivalent classes 'IsJSVal' and 'PToJSVal'
-- Eg. You can convert ((PureJSVal Object) to JSVal with pToJSVal without requiring IO)
newtype PureJSVal a = PureJSVal a
    deriving J.IsJSVal

instance J.IsJSVal a => J.PToJSVal (PureJSVal a) where
    pToJSVal (PureJSVal a) = J.jsval a

type Property = (T.Text, J.JSVal)

toMaybeJSObject :: [Property] -> IO (Maybe JO.Object)
toMaybeJSObject [] = pure Nothing
toMaybeJSObject xs = Just <$> toJSObject xs

toJSObject :: [Property] -> IO JO.Object
toJSObject [] = JO.create
toJSObject xs = do
    obj <- JO.create
    traverse_ (\(k, v) -> JO.unsafeSetProp (J.textToJSString k) v obj) xs
    pure obj
