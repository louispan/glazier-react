{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Maker where

import Control.Concurrent.MVar
import Control.Monad.Free.Class
import Control.Monad.Free.Church
import Control.Monad.Free.TH
import Control.Monad.Trans.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R

-- | DSL for IO effects required during making widget models and callbacks
-- 'Maker' remembers the action type to allow 'withAction' for changing the action type by parent widgets.
-- The model type does not need to be changed, so it is hidden in the GADT existential.
data Maker act nxt where
    MkHandler
        :: (J.JSVal -> MaybeT IO [act])
        -> (J.Callback (J.JSVal -> IO ()) -> nxt)
        -> Maker act nxt
    MkEmptyFrame
        :: (MVar (R.Model dtl pln) -> nxt)
        -> Maker act nxt
    MkRenderer
        :: MVar mdl
        -> (J.JSVal -> G.WindowT mdl R.ReactMl ())
        -> (J.Callback (J.JSVal -> IO J.JSVal) -> nxt)
        -> Maker act nxt
    PutFrame
        :: MVar (R.Model dtl pln)
        -> R.Model dtl pln
        -> nxt
        -> Maker act nxt
    GetComponent
        :: (R.ReactComponent -> nxt)
        -> Maker act nxt
    MkKey
        :: (J.JSString -> nxt)
        -> Maker act nxt

instance Functor (Maker act) where
  fmap f (MkHandler handler g) = MkHandler handler (f . g)
  fmap f (MkEmptyFrame g) = MkEmptyFrame (f . g)
  fmap f (MkRenderer frm render g) = MkRenderer frm render (f . g)
  fmap f (PutFrame frm dsn x) = PutFrame frm dsn (f x)
  fmap f (GetComponent g) = GetComponent (f . g)
  fmap f (MkKey g) = MkKey (f . g)

makeFree ''Maker

-- | Allows changing the action type of Maker
withAction :: (act -> act') -> Maker act a -> Maker act' a
withAction f (MkHandler handler g) = MkHandler (\v -> fmap f <$> handler v) g
withAction _ (MkEmptyFrame g) = MkEmptyFrame g
withAction _ (MkRenderer frm render g) = MkRenderer frm render g
withAction _ (PutFrame frm scn x) = PutFrame frm scn x
withAction _ (GetComponent g) = GetComponent g
withAction _ (MkKey g) = MkKey g

hoistWithAction :: (act -> act') -> F (Maker act) a -> F (Maker act') a
hoistWithAction f = hoistF (withAction f)
