{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReadIORef.Internal where

import Data.IORef

-- | NB. Don't export ReadSubject constructor to guarantee
-- that that it only contains non-blocking 'readIORef' IO.
newtype ReadIORef a = ReadIORef (IO a)
    deriving (Functor, Applicative, Monad)

instance Show (ReadIORef a) where
    showsPrec _ (ReadIORef _) = showString "ReadIORef"

unReadIORef :: ReadIORef a -> IO a
unReadIORef (ReadIORef m) = m

doReadIORef :: IORef a -> ReadIORef a
doReadIORef ref = ReadIORef $ readIORef $ ref
