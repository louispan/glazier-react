{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Dispose
    ( Disposable -- constructor not exported
    , runDisposable
    , Dispose(..)
    , GDispose(..)
    ) where

import Control.Monad
import Control.Concurrent.STM
import qualified Data.DList as DL
import Data.IORef
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras.JSVar as JE

-- | A wrapper around authorized IO actions.
newtype Disposable a = Disposable { runDisposable :: Maybe (IO a) }
    deriving (Functor) -- , Applicative, Monad, Monoid)

instance Semigroup (Disposable ()) where
    (Disposable Nothing) <> f = f
    f <> (Disposable Nothing) = f
    (Disposable (Just f)) <> (Disposable (Just g)) = Disposable (Just (f >> g))

instance Monoid (Disposable ()) where
    mempty = Disposable Nothing
    mappend = (<>)

-- | A 'Dispose' is something with some resources tod release
class Dispose a where
    dispose :: a -> Disposable ()
    default dispose :: (G.Generic a, GDispose (G.Rep a)) => a -> Disposable ()
    dispose x = gDispose $ G.from x

instance Dispose (Disposable a) where
    dispose = void

-- | Generic instance basically traverses the data type structure
-- and expects the values to be all instances of 'Dispose'
class GDispose f where
    gDispose :: f p -> Disposable ()

instance GDispose G.U1 where
  gDispose G.U1 = mempty

instance (GDispose f, GDispose g) => GDispose (f G.:+: g) where
  gDispose (G.L1 x) = gDispose x
  gDispose (G.R1 x) = gDispose x

instance (GDispose f, GDispose g) => GDispose (f G.:*: g) where
  gDispose (x G.:*: y) = gDispose x <> gDispose y

instance (Dispose c) => GDispose (G.K1 i c) where
  gDispose (G.K1 x) = dispose x

instance (GDispose f) => GDispose (G.M1 i t f) where
  gDispose (G.M1 x) = gDispose x

------------------------------

instance Dispose (J.Callback a) where
    dispose = Disposable . Just . J.releaseCallback

instance Dispose J.JSString where
    dispose _ = mempty

instance Dispose JE.JSVar where
    dispose _ = mempty

instance (Dispose a, Dispose b) => Dispose (a, b) where
    dispose (a, b) = dispose a <> dispose b

instance (Dispose a, Dispose b, Dispose c) => Dispose (a, b, c) where
    dispose (a, b, c) = dispose a <> dispose b <> dispose c

instance (Dispose a, Dispose b, Dispose c, Dispose d) => Dispose (a, b, c, d) where
    dispose (a, b, c, d) = dispose a <> dispose b <> dispose c <> dispose d

instance Dispose a => Dispose [a] where
    dispose = foldMap dispose

instance Dispose a => Dispose (DL.DList a) where
    dispose = foldMap dispose

instance Dispose a => Dispose (Maybe a) where
    dispose = foldMap dispose

instance Dispose Int where
    dispose _ = mempty

instance Dispose J.JSVal where
    dispose _ = mempty

instance Dispose a => Dispose (TVar a) where
    dispose a = Disposable $ Just $ do
        a' <- readTVarIO a
        let Disposable f = dispose a' in case f of
           Nothing -> pure ()
           Just f' -> f'

instance Dispose a => Dispose (TMVar a) where
    dispose a = Disposable $ Just $ do
        a' <- atomically $ readTMVar a
        let Disposable f = dispose a' in case f of
           Nothing -> pure ()
           Just f' -> f'

instance Dispose a => Dispose (IORef a) where
    dispose a = Disposable $ Just $ do
        a' <- readIORef a
        let Disposable f = dispose a' in case f of
           Nothing -> pure ()
           Just f' -> f'
