{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Glazier.React.Entity where

import Control.Lens
import Glazier.React.Obj

----------------------------------------------------------------------------------

-- | Contains a 'WeakObj' pointer to a data structure,
-- and a 'Traversal'' to focus on a subset of the data structure.
data Entity o s = Entity { this :: Traversal' o s,  self :: WeakObj o }

-- instance GetWeakObj (Entity o s) o where
--     weakObj (Entity _ o) = o

_this :: Lens (Entity o s) (Entity o s') (ReifiedTraversal' o s) (ReifiedTraversal' o s')
_this = lens (\(Entity s _) -> Traversal s) (\(Entity _ o) (Traversal t) -> Entity t o)




magnifiedEntity ::
    ( Magnify m n (Entity o a) (Entity o b)
    , Contravariant (Magnified m r)
    )
    => Traversal' b a -> m r -> n r
magnifiedEntity l = magnify (to go)
  where
    go Entity {..} = Entity (this.l) self
