{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Glazier.React.Gadget where

import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Trans.AReader
import Control.Monad.Trans.AState.Strict
import qualified Data.DList as DL
import Glazier.React.Subject

type GadgetT cmd p s m = AReaderT (Entity p s) (AContT () (AStateT (DL.DList cmd) m))
type Gadget cmd p s = GadgetT cmd p s Identity

-- data Gadget2 r c p s a where
--     Gadget2 ::
--         ( HasItem (ReifiedTraversal' p s) r
--         , HasItem (Subject p) r
--         )
--         => AReaderT r (AContT () (AState (Scenario c p))) a
--         -> Gadget2 r c p s a

-- unGadget2 ::
--     ( HasItem (ReifiedTraversal' p s) r
--     , HasItem (Subject p) r
--     ) => Gadget2 r c p s a -> AReaderT r (AContT () (AState (Scenario c p))) a
-- unGadget2 (Gadget2 m) = m

-- instance Functor (Gadget2 r c p s) where
--     fmap f (Gadget2 m) = Gadget2 $ fmap f m

-- instance
--     ( HasItem (ReifiedTraversal' p s) r
--     , HasItem (Subject p) r
--     ) => Applicative (Gadget2 r c p s) where
--     pure = Gadget2 . pure
--     (Gadget2 f) <*> (Gadget2 a) = Gadget2 $ f <*> a

-- instance
--     ( HasItem (ReifiedTraversal' p s) r
--     , HasItem (Subject p) r
--     ) => Monad (Gadget2 r c p s) where
--     (Gadget2 f) >>= k = Gadget2 $ f >>= (unGadget2 . k)

-- instance ( HasItem (ReifiedTraversal' p s) r
--     , HasItem (Subject p) r
--     ) => MonadReader r (Gadget2 r c p s) where
--     ask = Gadget2 ask

gadgetT ::
    (Entity p s
        -> (a -> AStateT (DL.DList cmd) m ())
        -> AStateT (DL.DList cmd) m ())
    -> GadgetT cmd p s m a
gadgetT f = areaderT (\r -> acontT (f r))

-- gadget ::
--     (Entity p s
--         -> (a -> AState (Scenario c p) ())
--         -> AState (Scenario c p) ())
--     -> Gadget c p s a
-- gadget = gadgetT

-- viewSelf :: forall s r c p.
--     ( HasItem (ReifiedTraversal' p s) r
--     )
--     => Gadget r c p (ReifiedTraversal' p s)
-- viewSelf = view item

-- viewSubject ::
--     ( HasItem (Subject p) r
--     )
--     => Gadget r c p (Subject p)
-- viewSubject = view item

-- magnifySelf2 :: forall p s a rs ra r c b proxy.
--     ( UniqueMember (ReifiedTraversal' p s) rs
--     , UniqueMember (ReifiedTraversal' p a) ra
--     , ra ~ Replace (ReifiedTraversal' p s) (ReifiedTraversal' p a) rs
--     )
--     => proxy p -> Traversal' s a -> Gadget (Many ra) c p b -> Gadget (Many rs) c p b
-- magnifySelf2 p l = magnify (to $ item %~ go p l)
--   where
--     go :: forall p s a proxy. proxy p -> Traversal' s a -> ReifiedTraversal' p s -> ReifiedTraversal' p a
--     go _ l' (Traversal s) = Traversal (s.l')


-- viewSelf' :: forall s r c p.
--     ( HasItem (ReifiedTraversal' p s) r
--     , HasItem (Subject p) r
--     )
--     => Gadget2 r c p s (ReifiedTraversal' p s)
-- viewSelf' = view item

-- viewSubject' ::
--     ( HasItem (Subject p) r
--     )
--     => Gadget2 r c p s (Subject p)
-- viewSubject' = view item

-- runGadgetT ::
--     GadgetT w x s m a
--     -> Traversal' w (Scene x s)
--     -> (AStateT w m a -> AStateT w m ())
--     -> AStateT w m ()
-- -- runMethodT' = (runDelegateT' .) . runAReaderT'
-- runGadgetT x l = runMContT (runAReaderT x (Traversal l))

runGadgetT ::
    GadgetT cmd p s m a
    -> Entity p s
    -> (a -> AStateT (DL.DList cmd) m ())
    -> AStateT (DL.DList cmd) m ()
runGadgetT x l = runAContT (runAReaderT x l)

-- runGadget ::
--     Gadget c p s a
--     -> Entity p s
--     -> (a -> AState (Scenario c p) ())
--     -> AState (Scenario c p) ()
-- runGadget = runGadgetT
