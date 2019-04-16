{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Subject where

import Control.Lens
import Glazier.React.Obj
import Glazier.React.ReactId

-- | Contains a 'WeakObj' pointer to a data structure,
-- and a 'Traversal'' to focus on a subset of the data structure.
data Subject o s = Subject
    { self :: WeakObj o
    , subject :: Traversal' o s
    , subjectId :: ReactId
    }

instance GetWeakObj o (Subject o s) where
    _getWeakObj = to self

magnifiedSubject ::
    ( Magnify m n (Subject o a) (Subject o b)
    , Contravariant (Magnified m r)
    )
    => Traversal' b a -> m r -> n r
magnifiedSubject l = magnify (to go)
  where
    go (Subject slf sbj sid) = Subject slf (sbj.l) sid
