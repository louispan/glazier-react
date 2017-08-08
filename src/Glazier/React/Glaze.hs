{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Glaze where

import Control.Concurrent.STM
import Control.Monad.Free.Class
import Control.Monad.Free.Church
import Control.Monad.Free.TH
import Control.Monad.Trans.Maybe
import qualified Data.JSString as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R

-- | DSL for IO effects required during making widget models and callbacks (which knows about the action type)
-- 'Glaze' remembers the action type to allow 'withAction' for changing the action type by parent widgets.
-- The model type does not need to be changed, so it is hidden in the GADT existential.
data Glaze act nxt where
    MkHandler
        :: (J.JSVal -> MaybeT IO [act]) -- this is why we need @act@ type variable
        -> (J.Callback (J.JSVal -> IO ()) -> nxt)
        -> Glaze act nxt
    MkRenderer
        :: G.WindowT mdl (R.ReactMlT STM) ()
        -> mdl
        -> (J.Callback (IO J.JSVal) -> nxt)
        -> Glaze act nxt
    GetComponent
        :: (R.ReactComponent -> nxt)
        -> Glaze act nxt
    MkKey
        :: (Int -> nxt)
        -> Glaze act nxt
    MkTVar
        :: mdl
        -> (TVar mdl -> nxt)
        -> Glaze act nxt
    ChangeTVar
        :: TVar mdl
        -> (mdl -> mdl)
        -> nxt
        -> Glaze act nxt
    SendAction
        :: act  -- this is why we need @act@ type variable
        -> nxt
        -> Glaze act nxt

instance Functor (Glaze act) where
  fmap f (MkHandler handler g) = MkHandler handler (f . g)
  fmap f (MkRenderer render frm g) = MkRenderer render frm (f . g)
  fmap f (GetComponent g) = GetComponent (f . g)
  fmap f (MkKey g) = MkKey (f . g)
  fmap f (MkTVar a g) = MkTVar a (f . g)
  fmap f (ChangeTVar v h x) = ChangeTVar v h (f x)
  fmap f (SendAction a x) = SendAction a (f x)

makeFree ''Glaze

-- | Allows changing the action type of Glaze
withAction :: (act -> act') -> Glaze act a -> Glaze act' a
withAction f (MkHandler handler g) = MkHandler (\v -> fmap f <$> handler v) g
withAction _ (MkRenderer render frm g) = MkRenderer render frm g
withAction _ (GetComponent g) = GetComponent g
withAction _ (MkKey g) = MkKey g
withAction _ (MkTVar a g) = MkTVar a g
withAction _ (ChangeTVar v h x) = ChangeTVar v h x
withAction f (SendAction a x) = SendAction (f a) x

hoistWithAction :: (act -> act') -> F (Glaze act) a -> F (Glaze act') a
hoistWithAction f = hoistF (withAction f)

-- | Like 'mkHandler'' but for a single @act@ instead of @[act]@.
mkHandler' :: (J.JSVal -> MaybeT IO act) -> F (Glaze act) (J.Callback (J.JSVal -> IO ()))
mkHandler' f = mkHandler (fmap pure <$> f)

mkKey' :: F (Glaze a) J.JSString
mkKey' = (J.pack . show) <$> mkKey
