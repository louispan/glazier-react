{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Util
    ( strval
    , getProperty
    , Trash(..)
    , Garbage(..)
    , scrap
    , classNames
    ) where

import qualified GHCJS.Types as J
import qualified GHCJS.Foreign.Callback as J
import qualified Data.JSString as J

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


-- | Allows storing releasables in a heterogenous container
class Trash a where
    trash :: a -> IO ()

data Garbage = forall a. Trash a => Garbage a

scrap :: Trash a => a -> Garbage
scrap = Garbage

instance Trash (J.Callback a) where
    trash = J.releaseCallback

classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = J.jsval . J.unwords . fmap fst . filter snd
