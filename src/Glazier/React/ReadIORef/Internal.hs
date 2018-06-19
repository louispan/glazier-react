{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReadIORef.Internal where

import Control.Applicative
import Data.IORef

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

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
#if !MIN_VERSION_base(4,11,0)
    (ReadIORef f) `mappend` (ReadIORef g) = ReadIORef (liftA2 mappend f g)
#endif

unReadIORef :: ReadIORef a -> IO a
unReadIORef (ReadIORef m) = m

doReadIORef :: IORef a -> ReadIORef a
doReadIORef ref = ReadIORef $ readIORef $ ref
