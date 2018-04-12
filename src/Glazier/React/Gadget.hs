{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Glazier.React.Gadget where

import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.AReader
import Control.Monad.Trans.AState.Strict
import Glazier.React.Scene

type MonadGadget r c p n m =
    ( MonadDelegate (n ()) m
    , MonadState (Scenario c p) n
    , MonadReader r m
    )

type GadgetT r c p m = AReaderT r (AContT () (AStateT (Scenario c p) m))
type Gadget r c p = GadgetT r c p Identity

gadgetT ::
    (r
        -> (a -> AStateT (Scenario c p) m ())
        -> AStateT (Scenario c p) m ())
    -> GadgetT r c p m a
gadgetT f = areaderT (\r -> acontT (f r))

-- runGadgetT ::
--     GadgetT w x s m a
--     -> Traversal' w (Scene x s)
--     -> (AStateT w m a -> AStateT w m ())
--     -> AStateT w m ()
-- -- runMethodT' = (runDelegateT' .) . runAReaderT'
-- runGadgetT x l = runMContT (runAReaderT x (Traversal l))

runGadgetT ::
    GadgetT r c p m a
    -> r
    -> (a -> AStateT (Scenario c p) m ())
    -> AStateT (Scenario c p) m ()
runGadgetT x l = runAContT (runAReaderT x l)


