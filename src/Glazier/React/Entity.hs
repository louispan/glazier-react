{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Entity where

import Control.Lens
import Glazier.React.Subject

----------------------------------------------------------------------------------

data Entity p s = Entity (Subject p) (Lens' p s)

_subject :: Lens' (Entity p s) (Subject p)
_subject = lens (\(Entity p _) -> p) (\(Entity _ s) p -> Entity p s)

_self :: Lens (Entity p s) (Entity p s') (ReifiedLens' p s) (ReifiedLens' p s')
_self = lens (\(Entity _ s) -> Lens s) (\(Entity p _) (Lens t) -> Entity p t)

magnifiedEntity ::
    ( Magnify m n (Entity p a) (Entity p b)
    , Contravariant (Magnified m r)
    )
    => Lens' b a -> m r -> n r
magnifiedEntity l = magnify (to go)
  where
    go (Entity sbj slf) = Entity sbj (slf.l)
