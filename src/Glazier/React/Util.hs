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
    , Disposable(..)
    , SomeDisposable(..)
    , Disposing(..)
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

-- | A 'Disposable' is something with some resources to release
class Disposable a where
    dispose :: a -> IO ()

instance Disposable (J.Callback a) where
    dispose = J.releaseCallback

-- | Allows storing 'Disposable's in a heterogenous container
data SomeDisposable where
    Dispose :: forall a. Disposable a => a -> SomeDisposable
    DisposeList :: forall a. Disposable a => [a] -> SomeDisposable

instance Disposable SomeDisposable where
    dispose (Dispose a) = dispose a
    dispose (DisposeList as) = traverse_ dispose as

-- | Allow generic deriving instances of things that can be made into 'SomeDisposable'
-- If a data type derives from Generic, and only contain instances of Disposable,
-- then it can be made an instance of Disposing.
class Disposing a where
  disposing :: a -> SomeDisposable
  default disposing :: (G.Generic a, GDisposing (G.Rep a)) => a -> SomeDisposable
  disposing x = DisposeList . D.toList . gDisposing $ G.from x

-- | Generics instance basically traverses the data tree
-- and expects the values to be all instances of 'Disposable'
class GDisposing f where
    gDisposing :: f p -> D.DList SomeDisposable

instance GDisposing G.U1 where
  gDisposing G.U1 = mempty

instance (GDisposing f, GDisposing g) => GDisposing (f G.:+: g) where
  gDisposing (G.L1 x) = gDisposing x
  gDisposing (G.R1 x) = gDisposing x

instance (GDisposing f, GDisposing g) => GDisposing (f G.:*: g) where
  gDisposing (x G.:*: y) = (gDisposing x) <> (gDisposing y)

instance (Disposable c) => GDisposing (G.K1 i c) where
  gDisposing (G.K1 x) = D.singleton $ Dispose x

instance (GDisposing f) => GDisposing (G.M1 i t f) where
  gDisposing (G.M1 x) = gDisposing x
