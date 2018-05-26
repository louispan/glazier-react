{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReadIORef.Internal where

import Control.Applicative
import Data.IORef
import Data.Semigroup

-- | NB. Don't export ReadSubject constructor to guarantee
-- that that it only contains non-blocking 'readIORef' IO.
newtype ReadIORef a = ReadIORef (IO a)
    deriving (Functor, Applicative, Monad)

instance Show (ReadIORef a) where
    showsPrec _ (ReadIORef _) = showString "ReadIORef"

instance Semigroup a => Semigroup (ReadIORef a) where
    ReadIORef f <> ReadIORef g = ReadIORef $ liftA2 (<>) f g

instance Monoid a => Monoid (ReadIORef a) where
    mempty = ReadIORef $ pure mempty
    ReadIORef f `mappend` ReadIORef g = ReadIORef $ f *> g

unReadIORef :: ReadIORef a -> IO a
unReadIORef (ReadIORef m) = m

doReadIORef :: IORef a -> ReadIORef a
doReadIORef ref = ReadIORef $ readIORef $ ref
