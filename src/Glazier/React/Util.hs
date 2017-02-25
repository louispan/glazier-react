{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Util
    ( strval
    , getProperty
    , classNames
    , Scrap(..)
    , Garbage(..)
    , Trash(..)
    ) where

import Data.Foldable
import qualified GHCJS.Types as J
import qualified GHCJS.Foreign.Callback as J
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified Data.DList as D
import Data.Semigroup

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

-- | A 'Scrap' is something with some resources to release
class Scrap a where
    scrap :: a -> IO ()

instance Scrap (J.Callback a) where
    scrap = J.releaseCallback

-- | Allows storing 'Scrap's in a heterogenous container
data Garbage where
    Rubbish :: forall a. Scrap a => a -> Garbage
    RubbishPile :: forall a. Scrap a => [a] -> Garbage

instance Scrap Garbage where
    scrap (Rubbish a) = scrap a
    scrap (RubbishPile as) = traverse_ scrap as

-- | Allow generic deriving instances of things that can be made into 'Garbage'
class Trash a where
  trash :: a -> Garbage
  default trash :: (G.Generic a, GTrash (G.Rep a)) => a -> Garbage
  trash x = RubbishPile . D.toList . gTrash $ G.from x

-- | All Scraps are Trash, and can be made into Garbage
-- instance Scrap a => Trash a where
--     trash = Rubbish

-- | Generics instance basically traverses the data tree
-- and expects the values to be all instances of Trash
class GTrash f where
    gTrash :: f p -> D.DList Garbage

instance GTrash G.U1 where
  gTrash G.U1 = mempty

instance (GTrash f, GTrash g) => GTrash (f G.:+: g) where
  gTrash (G.L1 x) = gTrash x
  gTrash (G.R1 x) = gTrash x

instance (GTrash f, GTrash g) => GTrash (f G.:*: g) where
  gTrash (x G.:*: y) = (gTrash x) <> (gTrash y)

instance (Scrap c) => GTrash (G.K1 i c) where
  gTrash (G.K1 x) = D.singleton $ Rubbish x

instance (GTrash f) => GTrash (G.M1 i t f) where
  gTrash (G.M1 x) = gTrash x
