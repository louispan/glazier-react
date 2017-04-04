{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Model where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import qualified GHC.Generics as G

-- | Lens to the callbacks and interactions with React
class HasPlan c pln | c -> pln where
    plan :: Lens' c pln

-- | Lens to the pure model for state and rendering.
class HasModel c mdl | c -> mdl where
    model :: Lens' c mdl

-- | Convert to the pure serializable model for saving and restoring
class ToOutline c o | c -> o where
    outline :: c -> o

-- | A record of Model and Plan
data Scene mdl pln = Scene
    { _model :: mdl
    , _plan :: pln
    } deriving (G.Generic)

class HasScene c mdl pln | c -> mdl pln where
    scene :: Lens' c (Scene mdl pln)

instance HasScene (Scene mdl pln) mdl pln where
    scene = id
    {-# INLINE scene #-}

-- | All scenes should be disposable to make it easier for cleanup of callbacks.
instance (CD.Disposing pln, CD.Disposing mdl) => CD.Disposing (Scene mdl pln)

instance HasPlan (Scene mdl pln) pln where
    plan f (Scene mdl pln) = fmap (\pln' -> Scene mdl pln') (f pln)
    {-# INLINE plan #-}

instance HasModel (Scene mdl pln) mdl where
    model f (Scene mdl pln) = fmap (\mdl' -> Scene mdl' pln) (f mdl)
    {-# INLINE model #-}

-- | Outline just uses the Model
instance ToOutline mdl o => ToOutline (Scene mdl pln) o where
    outline = view (model . to outline)
    {-# INLINE outline #-}

-- | Frame is a Mvar of Scene. React rendering callback uses this MVar for rendering.
type Frame mdl pln = MVar (Scene mdl pln)

class HasFrame c mdl pln | c -> mdl pln where
    frame :: Lens' c (Frame mdl pln)

instance HasFrame (Frame mdl pln) mdl pln where
    frame = id
    {-# INLINE frame #-}

-- | A record of Scene and Frame.
data Gizmo mdl pln = Gizmo
    { _scene :: Scene mdl pln
    , _frame :: Frame mdl pln
    } deriving (G.Generic)

-- | Undecidableinstances!
-- But this is safe because Scene is definitely smaller than Gizmo
instance CD.Disposing (Scene mdl pln) => CD.Disposing (Gizmo mdl pln) where
    disposing s = CD.disposing $ s ^. scene
    {-# INLINE disposing #-}

class (HasScene c mdl pln, HasFrame c mdl pln) => HasGizmo c mdl pln | c -> mdl pln where
    gizmo :: Lens' c (Gizmo mdl pln)

instance HasGizmo (Gizmo mdl pln) mdl pln where
    gizmo = id
    {-# INLINE gizmo #-}

instance HasFrame (Gizmo mdl pln) mdl pln where
    frame f (Gizmo scn frm) = fmap (\frm' -> Gizmo scn frm') (f frm)
    {-# INLINE frame #-}

instance HasScene (Gizmo mdl pln) mdl pln where
    scene f (Gizmo scn frm) = fmap (\scn' -> Gizmo scn' frm) (f scn)
    {-# INLINE scene #-}

instance HasPlan (Gizmo mdl pln) pln where
    plan = scene . plan
    {-# INLINE plan #-}

instance HasModel (Gizmo mdl pln) mdl where
    model = scene . model
    {-# INLINE model #-}

-- | Outline just uses the Model
instance ToOutline mdl o => ToOutline (Gizmo mdl pln) o where
    outline = view (model . to outline)
    {-# INLINE outline #-}
