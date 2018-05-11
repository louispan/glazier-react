{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Subject.Internal where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.IORef
import Glazier.React.Scene
----------------------------------------------------------------------------------

-- | Using MVar for synchronizing because it guarantees FIFO wakeup
-- which will help prevent old updates overriding new updates.
-- The constructor is hidden to maintain the expectation that
-- the 'lease' member will keep the 'ShimCallbacks' inside the 'Scene' alive.
-- Before removing Subject from a container, use 'prolong' to store
-- the prolonging IO action "onceOnRendered" action handler,
-- so that the 'ShimCallbacks' is garbage collected
-- only after the child widget is no longer rendered.

data Subject p = Subject (IORef (Scene p)) (MVar (Scene p)) (MVar ())
    deriving Eq

sceneRef :: Subject p -> IORef (Scene p)
sceneRef (Subject x _ _) = x

sceneVar :: Subject p -> MVar (Scene p)
sceneVar (Subject _ x _) = x

-- | Creates an IO action whose existence will keep the lease alive.
-- Running it has no side effects.
prolong :: Subject r -> IO ()
prolong (Subject _ _ r) = (void $ isEmptyMVar r)

data Entity p s = Entity (Subject p) (Traversal' p s)

_subject :: Lens' (Entity p s) (Subject p)
_subject = lens (\(Entity p _) -> p) (\(Entity _ s) p -> Entity p s)

_self :: Lens (Entity p s) (Entity p s') (ReifiedTraversal' p s) (ReifiedTraversal' p s')
_self = lens (\(Entity _ s) -> Traversal s) (\(Entity p _) (Traversal t) -> Entity p t)

magnifySelf ::
    ( Magnify m n (Entity p a) (Entity p b)
    , Contravariant (Magnified m r)
    )
    => Traversal' b a -> m r -> n r
magnifySelf l = magnify (to go)
  where
    go (Entity sbj slf) = Entity sbj (slf.l)

magnifyModel ::
    ( Magnify m n (Scene a) (Scene b)
    , Functor (Magnified m r)
    )
    => LensLike' (Magnified m r) b a -> m r -> n r
magnifyModel l = magnify (editSceneModel l)
