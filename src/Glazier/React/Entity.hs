{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Entity where

import Control.Lens
import Glazier.React.Obj

----------------------------------------------------------------------------------

data Entity p s = Entity (WeakObj p) (Traversal' p s)

_weakObj :: Lens' (Entity p s) (WeakObj p)
_weakObj = lens (\(Entity p _) -> p) (\(Entity _ s) p -> Entity p s)

_this :: Lens (Entity p s) (Entity p s') (ReifiedTraversal' p s) (ReifiedTraversal' p s')
_this = lens (\(Entity _ s) -> Traversal s) (\(Entity p _) (Traversal t) -> Entity p t)

magnifiedEntity ::
    ( Magnify m n (Entity p a) (Entity p b)
    , Contravariant (Magnified m r)
    )
    => Traversal' b a -> m r -> n r
magnifiedEntity l = magnify (to go)
  where
    go (Entity obj this) = Entity obj (this.l)
