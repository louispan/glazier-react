{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Reactor where

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
import qualified Pipes.Concurrent as PC

-- | DSL for IO effects required during making widget models and callbacks
-- FIMXE: Use MTL style instead of Free Monad?
data Reactor nxt where
    MkHandler
        :: PC.Output act
        -> (J.JSVal -> MaybeT IO [act])
        -> (J.Callback (J.JSVal -> IO ()) -> nxt)
        -> Reactor nxt
    MkRenderer
        :: G.WindowT s (R.ReactMlT STM) ()
        -> s
        -> (J.Callback (IO J.JSVal) -> nxt)
        -> Reactor nxt
    GetComponent
        :: (R.ReactComponent -> nxt)
        -> Reactor nxt
    MkKey
        :: (Int -> nxt)
        -> Reactor nxt
    DoNewEmptyTMVar
        :: (TMVar s -> nxt)
        -> Reactor nxt
    DoPutTMVar
        :: TMVar a
        -> a
        -> nxt
        -> Reactor nxt
    -- DoNewTVar
    --     :: s
    --     -> (TVar s -> nxt)
    --     -> Reactor nxt
    -- DoModifyTVar
    --     :: TVar s
    --     -> (s -> s)
    --     -> nxt
    --     -> Reactor nxt
    SendAction
        :: PC.Output act
        -> act
        -> nxt
        -> Reactor nxt

instance Functor Reactor where
  fmap f (MkHandler out handler g) = MkHandler out handler (f . g)
  fmap f (MkRenderer render frm g) = MkRenderer render frm (f . g)
  fmap f (GetComponent g) = GetComponent (f . g)
  fmap f (MkKey g) = MkKey (f . g)
  fmap f (DoNewEmptyTMVar g) = DoNewEmptyTMVar (f . g)
  fmap f (DoPutTMVar v h x) = DoPutTMVar v h (f x)
  -- fmap f (DoNewTVar s g) = DoNewTVar s (f . g)
  -- fmap f (DoModifyTVar v h x) = DoModifyTVar v h (f x)
  fmap f (SendAction o a x) = SendAction o a (f x)

makeFree ''Reactor

-- -- | Allows changing the action type of Reactor
-- withAction :: (act -> act') -> Reactor act a -> Reactor act' a
-- withAction f (MkHandler handler g) = MkHandler (\v -> fmap f <$> handler v) g
-- withAction _ (MkRenderer render frm g) = MkRenderer render frm g
-- withAction _ (GetComponent g) = GetComponent g
-- withAction _ (MkKey g) = MkKey g
-- withAction _ (MkTVar a g) = MkTVar a g
-- withAction _ (ChangeTVar v h x) = ChangeTVar v h x
-- withAction f (SendAction a x) = SendAction (f a) x

-- hoistWithAction :: (act -> act') -> F (Reactor act) a -> F (Reactor act') a
-- hoistWithAction f = hoistF (withAction f)

-- | Like 'mkHandler'' but for a single @act@ instead of @[act]@.
mkHandler' :: PC.Output act -> (J.JSVal -> MaybeT IO act) -> F Reactor (J.Callback (J.JSVal -> IO ()))
mkHandler' out f = mkHandler out (fmap pure <$> f)

mkKey' :: F Reactor J.JSString
mkKey' = (J.pack . show) <$> mkKey
