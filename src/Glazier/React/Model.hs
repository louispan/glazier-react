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
import qualified GHC.Generics as G

-- | Lens to the data for state and rendering.
class HasDetail c dtl | c -> dtl where
    detail :: Lens' c dtl

-- | Lens to the callbacks and interactions with React
class HasPlan c pln | c -> pln where
    plan :: Lens' c pln

-- | Convert to the serializable outline for saving and restoring
class ToOutline c o | c -> o where
    outline :: c -> o

---------------------------------------------

-- | A record of Detail and Plan
data Model dtl pln = Model
    { _detail :: dtl
    , _plan :: pln
    } deriving (G.Generic)

class HasModel c dtl pln | c -> dtl pln where
    model :: Lens' c (Model dtl pln)

instance HasModel (Model dtl pln) dtl pln where
    model = id
    {-# INLINE model #-}

-- | All models should be disposable to make it easier for cleanup of callbacks.
instance (CD.Disposing pln, CD.Disposing dtl) => CD.Disposing (Model dtl pln)

instance HasPlan (Model dtl pln) pln where
    plan f (Model dtl pln) = fmap (\pln' -> Model dtl pln') (f pln)
    {-# INLINE plan #-}

instance HasDetail (Model dtl pln) dtl where
    detail f (Model dtl pln) = fmap (\dtl' -> Model dtl' pln) (f dtl)
    {-# INLINE detail #-}

-- | A Model can be converted to Outline by using the Detail
-- | Undecidableinstances! This is safe because dtl is definitely smaller than (Model dtl pln)
instance ToOutline dtl ol => ToOutline (Model dtl pln) ol where
    outline = view (detail . to outline)
    {-# INLINE outline #-}

---------------------------------------------

class HasMVar c a | c -> a where
    mvar :: Lens' c (MVar a)

instance HasMVar (MVar a) a where
    mvar = id
    {-# INLINE mvar #-}

class HasIVal c a | c -> a where
    ival :: Lens' c a

----------------------------------------------------------
-- | This is used by the gadget to be able to purely manipulate a value
-- as well as put into an MVar for other threads to access the value.
newtype Shared a = Shared (MVar a, a)

makeWrapped ''Shared

-- | Undecidableinstances! This is safe because 'a' is definitely smaller than 'Shared a'
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

-- | Undecidableinstances! This is safe because (HasModel mdl dtl pln) is definitely smaller than (Shared mdl)
instance HasModel mdl dtl pln => HasModel (Shared mdl) dtl pln where
    model = ival . model
    {-# INLINE model #-}

-- | Undecidableinstances! This is safe because (HasModel mdl dtl pln) is definitely smaller than (Shared mdl)
instance HasModel mdl dtl pln => HasPlan (Shared mdl) pln where
    plan = model . plan
    {-# INLINE plan #-}

-- | Undecidableinstances! This is safe because (HasModel mdl dtl pln) is definitely smaller than (Shared mdl)
instance HasModel mdl dtl pln => HasDetail (Shared mdl) dtl where
    detail = model . detail
    {-# INLINE detail #-}

-- | A Entity can be converted to Outline by using the Detail
-- Undecidableinstances! This is safe because (HasModel mdl dtl pln) is definitely smaller than (Shared mdl)
instance (HasModel mdl dtl pln, ToOutline dtl ol) => ToOutline (Shared mdl) ol where
    outline = view (detail . to outline)
    {-# INLINE outline #-}
