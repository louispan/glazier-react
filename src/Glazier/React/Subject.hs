{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Glazier.React.Subject where

import Control.Also
import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Diverse.Lens
import Data.Profunctor.Unsafe
import Glazier.Command
import Glazier.React.Obj
import Glazier.React.ReactId

-- | Contains a 'WeakObj' pointer to a data structure,
-- and a 'Traversal'' to focus on a subset of the data structure.
data Subject o s = Subject
    { self :: WeakObj o
    , subject :: Traversal' o s
    -- , subjectId :: ReactId
    }

magnifiedSubject ::
    ( Magnify m n (Subject o a) (Subject o b)
    , Contravariant (Magnified m r)
    )
    => Traversal' b a -> m r -> n r
magnifiedSubject l = magnify (to go)
  where
    go (Subject slf sbj) = Subject slf (sbj.l)


-- instance GetWeakObj o (Subject o s) where
--     _getWeakObj = to self

-- | ReifiedTraversal because GHC doesn't support impredicative polymorphism
-- Not using a MonadReader pattern because the env type #ReifiedTraversal' o s@
-- may change with 'magnifySubjectT'
newtype SubjectT o s m a = SubjectT { runSubjectT :: ReaderT (ReifiedTraversal' o s) m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MFunctor
        , MonadState s'
        , MonadCont
        , MonadError e
        , MonadWriter w
        -- , MonadReader (ReifiedTraversal' o s)
        , MonadPlus
        , MonadFix

        , Alternative
        , Also a'
        , MonadDelegate
        , MonadCodify c
        , MonadProgram c

        -- , MonadGadgetId
        -- , MonadSelf
        )

type instance Magnified (SubjectT o s m) = Effect m
type instance Zoomed (SubjectT o s m) = Zoomed m

instance Zoom m n s t => Zoom (SubjectT o s' m) (SubjectT o s' n) s t where
  zoom l (SubjectT (ReaderT m)) = SubjectT (ReaderT (zoom l . m))

magnifySubjectT :: Monad m => Traversal' t s -> SubjectT o s m a -> SubjectT o t m a
magnifySubjectT l (SubjectT m) = SubjectT (magnify (to go) m)
  where
    go (Traversal sbj) = Traversal (sbj.l)

-- instance Monad m => Magnify (SubjectT o t m) (SubjectT o s m) (ReifiedTraversal' o t) (ReifiedTraversal' o s) where
--   magnify l (SubjectT (ReaderT m)) = SubjectT (ReaderT $ getEffect #. l (Effect #. m))

subjectT' :: (Traversal' o s -> m a) -> SubjectT o s m a
subjectT' f = SubjectT $ ReaderT (\a -> f (runTraversal a))

runSubjectT' :: SubjectT o s m a -> Traversal' o s -> m a
runSubjectT' m a = (`runReaderT` (Traversal a)) (runSubjectT m)

class Monad m => MonadReactId m where
    getReactId :: m ReactId
    default getReactId :: (MonadReactId m, MonadTrans t) => t m ReactId
    getReactId = lift getReactId

newtype ReactIdT m a = ReactIdT { runReactIdT :: ReaderT ReactId m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MFunctor
        , MonadDelegate
        , MonadCodify c
        , MonadProgram c
        , MonadState s'
        , MonadCont
        , MonadError e
        , MonadWriter w
        , Also a'
        , Alternative
        , MonadPlus
        , MonadFix
        )

instance Monad m => MonadReactId (ReactIdT m) where
    getReactId = ReactIdT ask

instance MonadReactId m => MonadReactId (SubjectT o s m)

instance MonadReactId m => MonadReactId (WeakObjT o m)



-- class Monad m => MonadSelf o m | m -> o where
--     self :: m (WeakObj o)
--     default self :: (MonadSelf o m, MonadTrans t) => t m (WeakObj o)
--     self = lift self

-- newtype WeakObjT o m a = WeakObjT { runWeakObjT :: ReaderT (WeakObj o) m a }
--     deriving (Functor, Applicative, Monad, MonadTrans, MFunctor
--         , MonadDelegate
--         , MonadCodify c
--         , MonadProgram c
--         , MonadState s'
--         , MonadCont
--         , MonadError e
--         , MonadWriter w
--         , Also a'
--         , Alternative
--         , MonadPlus
--         , MonadFix
--         )

-- instance Monad m => MonadSelf o (WeakObjT o m) where
--     self = WeakObjT ask

-- instance MonadSelf o m => MonadSelf o (SubjectT o s m)

-- instance MonadSelf o m => MonadSelf o (ReactIdT m)
