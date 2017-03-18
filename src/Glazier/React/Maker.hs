{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Maker where

import Control.Monad.Free.Class
import Control.Monad.Free.TH
import Control.Monad.Trans.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R

-- | DSL for IO effects required during making widget models and callbacks
-- 'Maker' remembers the action type to allow 'mapAction' for changing the action type by parent widgets.
-- The model type does not need to be changed, so it is hidden in the GADT existential.
data Maker act nxt where
    MkHandler
        :: (J.JSVal -> MaybeT IO [act])
        -> (J.Callback (J.JSVal -> IO ()) -> nxt)
        -> Maker act nxt
    MkEmptyFrame
        :: (R.Frame mdl pln -> nxt)
        -> Maker act nxt
    MkRenderer
        :: R.Frame mdl pln
        -> (J.JSVal -> G.WindowT (R.Design mdl pln) R.ReactMl ())
        -> (J.Callback (J.JSVal -> IO J.JSVal) -> nxt)
        -> Maker act nxt
    PutFrame
        :: R.Frame mdl pln
        -> R.Design mdl pln
        -> nxt
        -> Maker act nxt
    GetComponent
        :: (R.ReactComponent -> nxt)
        -> Maker act nxt

instance Functor (Maker act) where
  fmap f (MkHandler handler g) = MkHandler handler (f . g)
  fmap f (MkEmptyFrame g) = MkEmptyFrame (f . g)
  fmap f (MkRenderer ms render g) = MkRenderer ms render (f . g)
  fmap f (PutFrame frm dsn x) = PutFrame frm dsn (f x)
  fmap f (GetComponent g) = GetComponent (f . g)

makeFree ''Maker

-- | Allows changing the action type of Maker
mapAction :: (act -> act') -> Maker act a -> Maker act' a
mapAction f (MkHandler handler g) = MkHandler (\v -> fmap f <$> handler v) g
mapAction _ (MkEmptyFrame g) = MkEmptyFrame g
mapAction _ (MkRenderer ms render g) = MkRenderer ms render g
mapAction _ (PutFrame frm dsn x) = PutFrame frm dsn x
mapAction _ (GetComponent g) = GetComponent g
