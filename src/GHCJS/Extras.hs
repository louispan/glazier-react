{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- FIXME: Rename and move to Javascript.Extras
module GHCJS.Extras
    ( strval
    , Property
    , getProperty
    , setProperty
    , PureJSVal(..)
    , toMaybeJSObject
    , toJSObject
    ) where

import Data.Foldable
import qualified Data.JSString as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Object as JO

-- | This makes it easier to use OverloadedStrings with inputs that accept a JSVal that could be a JSString
strval :: J.JSString -> J.JSVal
strval = J.jsval

type Property = (J.JSString, J.JSVal)

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$2[$1]"
  js_unsafeGetProperty :: J.JSString -> J.JSVal -> IO J.JSVal

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$3[$1] = $2;"
  js_unsafeSetProperty :: J.JSString -> J.JSVal -> J.JSVal -> IO ()

-- | get a property of any JSVal. If a null or undefined is queried, the result will also be null
getProperty :: J.JSString -> J.JSVal -> IO J.JSVal
getProperty k x = let k' = J.pToJSVal k
                  in if J.isUndefined x || J.isNull x
                         || J.isUndefined k' || J.isNull k'
                     then pure J.nullRef
                     else js_unsafeGetProperty k x

-- | set a property of any JSVal
setProperty :: Property -> J.JSVal -> IO ()
setProperty (k, v) x = let k' = J.pToJSVal k
                    in if J.isUndefined x || J.isNull x
                          || J.isUndefined k' || J.isNull k'
                       then pure ()
                       else js_unsafeSetProperty k v x

-- | This is to enable conversion between equivalent classes 'IsJSVal' and 'PToJSVal'
-- So you can convert ((PureJSVal Object) to JSVal with pToJSVal without requiring IO)
newtype PureJSVal a = PureJSVal a
    deriving J.IsJSVal

instance J.IsJSVal a => J.PToJSVal (PureJSVal a) where
    pToJSVal (PureJSVal a) = J.jsval a

toMaybeJSObject :: [Property] -> IO (Maybe JO.Object)
toMaybeJSObject [] = pure Nothing
toMaybeJSObject xs = Just <$> toJSObject xs

toJSObject :: [Property] -> IO JO.Object
toJSObject [] = JO.create
toJSObject xs = do
    obj <- JO.create
    traverse_ (\(k, v) -> JO.unsafeSetProp k v obj) xs
    pure obj
