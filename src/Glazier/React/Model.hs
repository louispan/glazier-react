{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Model where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
-- import qualified GHC.Generics as G

-- | Convert to the serializable outline for saving and restoring
-- All Detail should be an instance of this
class ToOutline dtl ol | dtl -> ol where
    outline :: dtl -> ol

-- -- | Lens to the data for state and rendering.
-- class HasDetail mdl dtl | mdl -> dtl where
--     detail :: Lens' mdl dtl

-- -- | Lens to the callbacks and interactions with React
-- class HasPlan mdl pln | mdl -> pln where
--     plan :: Lens' mdl pln

---------------------------------------------

-- -- | A record of Detail and Plan
-- data Model dtl pln = Model
--     { _detail :: dtl
--     , _plan :: pln
--     }

-- detail :: Lens' (Model dtl pln) dtl
-- detail f (Model dtl pln) = fmap (\dtl' -> Model dtl' pln) (f dtl)
-- {-# INLINE detail #-}

-- plan :: Lens' (Model dtl pln) pln
-- plan f (Model dtl pln) = fmap (\pln' -> Model dtl pln') (f pln)
-- {-# INLINE plan #-}

-- class HasModel c dtl pln | c -> dtl pln where
--     model :: Lens' c (Model dtl pln)

-- instance HasModel (Model dtl pln) dtl pln where
--     model = id
--     {-# INLINE model #-}

-- -- | All models should be disposable to make it easier for cleanup of callbacks.
-- instance (CD.Disposing pln, CD.Disposing dtl) => CD.Disposing (Model dtl pln)

-- instance HasPlan (Model dtl pln) pln where
--     plan f (Model dtl pln) = fmap (\pln' -> Model dtl pln') (f pln)
--     {-# INLINE plan #-}

-- instance HasDetail (Model dtl pln) dtl where
--     detail f (Model dtl pln) = fmap (\dtl' -> Model dtl' pln) (f dtl)
--     {-# INLINE detail #-}

-- | A Model can be converted to Outline by using the Detail
-- UndecidableInstances:
-- The coverage condition fails in class ‘ToOutline’ for functional dependency: ‘dtl -> ol’
-- Reason: lhs type ‘Model dtl pln’ does not determine rhs type ‘ol'
-- NB: Safe because because 'Model dtl pln' determines dtl; in the constraints dtl determines ol.
-- instance ToOutline dtl ol => ToOutline (Model dtl pln) ol where
--     outline = view (detail . to outline)
--     {-# INLINE outline #-}

---------------------------------------------

-- | Record accessor for a MVar
class HasMVar c a | c -> a where
    mvar :: Lens' c (MVar a)

instance HasMVar (MVar a) a where
    mvar = id
    {-# INLINE mvar #-}

-- | Record accessor for a immutable value (as opposed to 'HasMVar')
class HasIVal c a | c -> a where
    ival :: Lens' c a

----------------------------------------------------------
-- | Something that has an immutable component, as well as a MVar that
-- can be used to share a value with other threads.
-- This is used by the gadget to be able to purely manipulate a value
-- as well as put it into an MVar for other threads to access the value.
newtype Shared a = Shared (MVar a, a)

makeWrapped ''Shared

instance CD.Disposing a => CD.Disposing (Shared a) where
    disposing (Shared (_, a)) = CD.disposing a
    {-# INLINE disposing #-}

-- class HasShared c a | c -> a where
--     shared :: Lens' c (Shared a)

-- instance HasShared (Shared a) a where
--     shared = id
--     {-# INLINE shared #-}

instance HasMVar (Shared mdl) mdl where
    mvar = _Wrapped' . _1
    {-# INLINE mvar #-}

instance HasIVal (Shared mdl) mdl where
    ival = _Wrapped' . _2
    {-# INLINE ival #-}

-- -- | Undecidableinstances! This is safe because (HasModel mdl dtl pln) is definitely smaller than (Shared mdl)
-- instance HasPlan mdl dtl pln => HasModel (Shared mdl) dtl pln where
--     model = ival . model
--     {-# INLINE model #-}

-- -- | UndecidableInstances:
-- -- The coverage condition fails in class ‘HasPlan’ for functional dependency: ‘mdl -> pln’
-- -- Reason: lhs type ‘Shared mdl’ does not determine rhs type ‘pln’
-- -- NB: Safe because 'Shared mdl' determines mdl; in the constraints mdl determines pln
-- instance HasPlan mdl pln => HasPlan (Shared mdl) pln where
--     plan = ival . plan
--     {-# INLINE plan #-}

-- -- | UndecidableInstances:
-- -- The coverage condition fails in class ‘HasDetail’ for functional dependency: ‘mdl -> dtl’
-- -- Reason: lhs type ‘Shared mdl’ does not determine rhs type ‘dtl’
-- -- NB: Safe because 'Shared mdl' determines mdl; in the constraints mdl determines dtl
-- instance HasDetail mdl dtl => HasDetail (Shared mdl) dtl where
--     detail = ival . detail
--     {-# INLINE detail #-}

-- -- | A Entity can be converted to Outline by using the Detail.
-- -- UndecidableInstances:
-- -- Variable ‘dtl’ occurs more often in the constraint ‘HasDetail mdl dtl’ than in the instance head
-- -- Variable ‘dtl’ occurs more often in the constraint ‘ToOutline dtl ol’ than in the instance head
-- -- The coverage condition fails in class ‘ToOutline’ for functional dependency: ‘dtl -> ol’
-- -- Reason: lhs type ‘Shared mdl’ does not determine rhs type ‘ol’
-- -- NB: Safe because 'Shared md' determins mdl; in the constraints mdl determines dtl, and dtl determins ol
-- instance (HasDetail mdl dtl, ToOutline dtl ol) => ToOutline (Shared mdl) ol where
--     outline = view (detail . to outline)
--     {-# INLINE outline #-}
