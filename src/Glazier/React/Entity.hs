{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Entity where

import Control.Lens
import Glazier.React.Obj

----------------------------------------------------------------------------------

data Entity o s = Entity (WeakObj o) (Traversal' o s)

instance GetWeakObj (Entity o s) o where
    weakObj (Entity o _) = o

_this :: Lens (Entity o s) (Entity o s') (ReifiedTraversal' o s) (ReifiedTraversal' o s')
_this = lens (\(Entity _ s) -> Traversal s) (\(Entity o _) (Traversal t) -> Entity o t)

magnifiedEntity ::
    ( Magnify m n (Entity o a) (Entity o b)
    , Contravariant (Magnified m r)
    )
    => Traversal' b a -> m r -> n r
magnifiedEntity l = magnify (to go)
  where
    go (Entity obj this) = Entity obj (this.l)
