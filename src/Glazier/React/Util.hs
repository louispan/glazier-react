{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Glazier.React.Util
    ( strval
    , getProperty
    , Scrap(..)
    , Garbage(..)
    -- , scrap
    , classNames
    ) where

import Data.Foldable
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

classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = J.jsval . J.unwords . fmap fst . filter snd

-- | Allows storing releasables in a heterogenous container
class Scrap a where
    scrap :: a -> IO ()

data Garbage where
    Trash :: forall a. Scrap a => a -> Garbage
    TrashPile :: forall a. Scrap a => [a] -> Garbage

instance Scrap (J.Callback a) where
    scrap = J.releaseCallback

instance Scrap Garbage where
    scrap (Trash a) = scrap a
    scrap (TrashPile as) = traverse_ scrap as
