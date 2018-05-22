{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.IORefReader.Internal where

import Data.IORef

-- | NB. Don't export ReadSubject constructor to guarantee
-- that that it only contains non-blocking 'readIORef' IO.
newtype IORefReader a = IORefReader (IO a)
    deriving (Functor, Applicative, Monad)

unIORefReader :: IORefReader a -> IO a
unIORefReader (IORefReader m) = m

doReadIORef :: IORef a-> IORefReader a
doReadIORef ref = IORefReader $ readIORef $ ref
