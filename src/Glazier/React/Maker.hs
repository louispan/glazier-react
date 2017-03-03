{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Maker where

import Control.Monad.Free.Class
import Control.Monad.Free.TH
import Control.Concurrent.MVar
import Control.Monad.Trans.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R

-- | DSL for IO effects required during making widget models and callbacks
data Maker act nxt where
    MkHandler :: (J.JSVal -> MaybeT IO act) -> (J.Callback (J.JSVal -> IO ()) -> nxt) -> Maker act nxt
    MkModelMVar :: (MVar mdl -> nxt) -> Maker act nxt
    MkRenderer :: MVar mdl -> (G.WindowT mdl (R.ReactMl) ()) -> (J.Callback (IO J.JSVal) -> nxt) -> Maker act nxt
    PutModelMVar :: MVar mdl -> mdl -> nxt -> Maker act nxt

instance Functor (Maker act) where
  fmap f (MkHandler handler g) = MkHandler handler (f . g)
  fmap f (MkModelMVar g) = MkModelMVar (f . g)
  fmap f (MkRenderer ms render g) = MkRenderer ms render (f . g)
  fmap f (PutModelMVar ms s x) = PutModelMVar ms s (f x)

makeFree ''Maker

-- | Allows changing the action type of Maker
mapAction :: (act -> act') -> Maker act a -> Maker act' a
mapAction f (MkHandler handler g) = MkHandler (\v -> f <$> handler v) g
mapAction _ (MkModelMVar g) = MkModelMVar g
mapAction _ (MkRenderer ms render g) = MkRenderer ms render g
mapAction _ (PutModelMVar ms s x) = PutModelMVar ms s x

mkMModel :: MonadFree (Maker act) m => (MVar mdl -> m cbs) -> (cbs -> mdl) -> m (MVar mdl, mdl)
mkMModel makeCallbacks createModel = do
    ms <- mkModelMVar
    cbs <- makeCallbacks ms
    let s = createModel cbs
    putModelMVar ms s
    pure (ms, s)
