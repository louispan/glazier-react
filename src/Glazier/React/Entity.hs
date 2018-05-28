{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Entity where

import Control.Lens
import Glazier.React.Scene
import Glazier.React.Subject

----------------------------------------------------------------------------------

data Entity p s = Entity (Subject p) (Lens' p s)

_subject :: Lens' (Entity p s) (Subject p)
_subject = lens (\(Entity p _) -> p) (\(Entity _ s) p -> Entity p s)

_self :: Lens (Entity p s) (Entity p s') (ReifiedLens' p s) (ReifiedLens' p s')
_self = lens (\(Entity _ s) -> Lens s) (\(Entity p _) (Lens t) -> Entity p t)

magnifySelf ::
    ( Magnify m n (Entity p a) (Entity p b)
    , Contravariant (Magnified m r)
    )
    => Lens' b a -> m r -> n r
magnifySelf l = magnify (to go)
  where
    go (Entity sbj slf) = Entity sbj (slf.l)

magnifyModel ::
    ( Magnify m n (Scene a) (Scene b)
    , Functor (Magnified m r)
    )
    => LensLike' (Magnified m r) b a -> m r -> n r
magnifyModel l = magnify (editSceneModel l)
