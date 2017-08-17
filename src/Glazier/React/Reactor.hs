{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Reactor where

import Control.Concurrent.STM
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.Free.TH
import qualified Data.JSString as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R
import qualified Pipes.Concurrent as PC

-- | DSL for IO effects required during making widget models and callbacks
-- FIMXE: Use MTL style instead of Free Monad?
data Reactor nxt where
    MkHandler
        :: (J.JSVal -> IO ())
        -> (J.Callback (J.JSVal -> IO ()) -> nxt)
        -> Reactor nxt
    MkRenderer
        :: R.ReactMlT STM ()
        -> (J.Callback (IO J.JSVal) -> nxt)
        -> Reactor nxt
    GetComponent
        :: (R.ReactComponent -> nxt)
        -> Reactor nxt
    MkKey
        :: (Int -> nxt)
        -> Reactor nxt

    DoSpawn
        :: PC.Buffer a
        -> ((PC.Output a, PC.Input a) -> nxt)
        -> Reactor nxt

    DoSTM
        :: STM a
        -> (a -> nxt)
        -> Reactor nxt

instance Functor Reactor where
  fmap f (MkHandler h g) = MkHandler h (f . g)
  fmap f (MkRenderer rnd g) = MkRenderer rnd (f . g)
  fmap f (GetComponent g) = GetComponent (f . g)
  fmap f (MkKey g) = MkKey (f . g)
  fmap f (DoSpawn b g) = DoSpawn b (f . g)
  fmap f (DoSTM m g) = DoSTM m (f . g)

makeFree ''Reactor

mkKey' :: F Reactor J.JSString
mkKey' = (J.pack . show) <$> mkKey

-- mkHandler' :: (J.JSVal -> MaybeT IO a) -> (a -> STM ()) -> (J.JSVal -> IO ())
-- mkHandler' trig f j = void . runMaybeT $ trig j >>= lift . atomically . f
