{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Subject.Internal where

import Control.Concurrent
import Control.Lens
import Data.IORef
import Glazier.React.Scene
----------------------------------------------------------------------------------

-- | Using MVar for synchronizing because it guarantees FIFO wakeup
-- which will help prevent old updates overriding new updates.
-- The constructor is hidden to maintain the expectation that
-- the 'prolong' member will keep the 'ShimCallbacks' inside the 'Scene' alive.
-- Before removing Subject from a container, store 'prolong' in the
-- "onceOnRendered" action handler, so that the 'ShimCallbacks' is garbage collected
-- only after the child widget is no longer rendered.

data Subject p = Subject
    -- | so we can have non-blocking reads
    { sceneRef :: IORef (Scene p)
    -- | canonical data
    , sceneVar :: MVar (Scene p)
    -- | The existence of prolong keeps the 'ShimCallbacks' inside the 'Scene' alive.
    -- Running it should effectively do nothing.
    , prolong_ :: IO ()
    }

prolong :: Subject p -> IO ()
prolong (Subject _ _ p) = p

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
