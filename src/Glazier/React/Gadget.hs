{-# LANGUAGE CPP #-}
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
import Control.Monad.Trans.Conts
import Control.Monad.Trans.Readers
import Control.Monad.Trans.States.Strict
import Glazier.React.Scene

-- | The type for initializing and handling callbacks.
-- @w@ world
-- @x@ commands to execute
-- @s@ actual model of widget
-- @m@ inner monad
-- @a@ return of monad

type GadgetT r c p m = ReadersT r (ContsT () (StatesT (Scenario c p) m))
type Gadget r c p = GadgetT r c p Identity

-- type GadgetT' r c p s m = HasItem (Arena p s) r => GadgetT r c p m

-- pattern Method' :: ((a -> m ()) -> m ()) -> DelegateT m a
-- pattern Method' f = DelegateT (ContT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE ReadersT_ #-}
-- #endif

-- readMy :: (Traversal' w (Scene x s) -> DelegateT (StateT w m) a) -> MethodT w x s m a
-- readMy f = do
--     Traversal my <- ask
--     lift $ f my

-- gadgetT ::
--     (Traversal' w (Scene x s)
--     -> (StatesT w m a -> StatesT w m ())
--     -> StatesT w m ())
--     -> GadgetT w x s m a
-- -- methodT' = readrT' . (delegateT' .) . (. runTraversal)
-- gadgetT f = readersT (\r -> MContT (f (runTraversal r)))

-- viewSelf ::
--     ( HasItem (ReifiedTraversal' p s) r
--     )
--     => GadgetT r c p s m (ReifiedTraversal' p s)
-- viewSelf = view item

-- magnifyArena :: forall p s a r c m b.
--     ( HasItem (Arena p s) r
--     )
--     => Traversal' s a -> GadgetT (Replaced (Arena p s) (Arena p a) r) c p a m b -> GadgetT r c p s m b
-- magnifyArena l = magnify (to $ item %~ restage @p l)

gadgetT ::
    (r
        -> (a -> StatesT (Scenario c p) m ())
        -> StatesT (Scenario c p) m ())
    -> GadgetT r c p m a
gadgetT f = readersT (\r -> contsT (f r))

-- runGadgetT ::
--     GadgetT w x s m a
--     -> Traversal' w (Scene x s)
--     -> (StatesT w m a -> StatesT w m ())
--     -> StatesT w m ()
-- -- runMethodT' = (runDelegateT' .) . runReadersT'
-- runGadgetT x l = runMContT (runReadersT x (Traversal l))

runGadgetT ::
    GadgetT r c p m a
    -> r
    -> (a -> StatesT (Scenario c p) m ())
    -> StatesT (Scenario c p) m ()
runGadgetT x l = runContsT (runReadersT x l)


