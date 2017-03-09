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
import qualified Glazier.React.Widget as R

-- | DSL for IO effects required during making widget models and callbacks
-- 'Maker' remembers the action type to allow 'mapAction' for changing the action type by parent widgets.
-- The model type does not need to be changed, so it is hidden in the GADT existential.
data Maker act nxt where
    MkHandler
        :: (J.JSVal -> MaybeT IO [act])
        -> (J.Callback (J.JSVal -> IO ()) -> nxt)
        -> Maker act nxt
    MkEmptyMModel
        :: (R.MModel gsk mdl -> nxt)
        -> Maker act nxt
    MkRenderer
        :: R.MModel gsk mdl
        -> (J.JSVal -> G.WindowT (R.GModel gsk mdl) (R.ReactMl) ())
        -> (J.Callback (J.JSVal -> IO J.JSVal) -> nxt)
        -> Maker act nxt
    PutMModel
        :: R.MModel gsk mdl
        -> R.GModel gsk mdl
        -> nxt
        -> Maker act nxt
    GetComponent
        :: (R.ReactComponent -> nxt)
        -> Maker act nxt

instance Functor (Maker act) where
  fmap f (MkHandler handler g) = MkHandler handler (f . g)
  fmap f (MkEmptyMModel g) = MkEmptyMModel (f . g)
  fmap f (MkRenderer ms render g) = MkRenderer ms render (f . g)
  fmap f (PutMModel ms s x) = PutMModel ms s (f x)
  fmap f (GetComponent g) = GetComponent (f . g)

makeFree ''Maker

-- | Allows changing the action type of Maker
mapAction :: (act -> act') -> Maker act a -> Maker act' a
mapAction f (MkHandler handler g) = MkHandler (\v -> fmap f <$> handler v) g
mapAction _ (MkEmptyMModel g) = MkEmptyMModel g
mapAction _ (MkRenderer ms render g) = MkRenderer ms render g
mapAction _ (PutMModel ms s x) = PutMModel ms s x
mapAction _ (GetComponent g) = GetComponent g

mkSuperModel
    :: MonadFree (Maker act) m
    => (R.MModel gsk mdl -> m gsk)
    -> (gsk -> R.GModel gsk mdl)
    -> m (R.SuperModel gsk mdl)
mkSuperModel makeGasket createGModel = do
    mm <- mkEmptyMModel
    g <- makeGasket mm
    let gm = createGModel g
    putMModel mm gm
    pure (R.SuperModel mm gm)
