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

-- | A record of Model and Plan
data Design mdl pln = Design
    { _model :: mdl
    , _plan :: pln
    } deriving (G.Generic)

-- | All designs should be disposable to make it easier for cleanup of callbacks.
instance (CD.Disposing pln, CD.Disposing mdl) => CD.Disposing (Design mdl pln)

instance HasPlan (Design mdl pln) pln where
    plan f (Design mdl pln) = fmap (\pln' -> Design mdl pln') (f pln)
    {-# INLINE plan #-}

instance HasModel (Design mdl pln) mdl where
    model f (Design mdl pln) = fmap (\mdl' -> Design mdl' pln) (f mdl)
    {-# INLINE model #-}

class HasDesign c mdl pln | c -> mdl pln where
    design :: Lens' c (Design mdl pln)

instance HasDesign (Design mdl pln) mdl pln where
    design = id

-- | Frame is a Mvar of Design. React rendering callback uses this MVar for rendering.
type Frame mdl pln = MVar (Design mdl pln)

class HasFrame c mdl pln | c -> mdl pln where
    frame :: Lens' c (Frame mdl pln)

instance HasFrame (Frame mdl pln) mdl pln where
    frame = id

-- | A record of Design and Frame.
data SuperModel mdl pln = SuperModel
    { _design :: Design mdl pln
    , _frame :: Frame mdl pln
    } deriving (G.Generic)

-- | Undecidableinstances!
-- But this is safe because Design is definitely smaller than SuperModel
instance CD.Disposing (Design mdl pln) => CD.Disposing (SuperModel mdl pln) where
    disposing s = CD.disposing $ s ^. design

class (HasDesign c mdl pln, HasFrame c mdl pln) => HasSuperModel c mdl pln | c -> mdl pln where
    superModel :: Lens' c (SuperModel mdl pln)

instance HasSuperModel (SuperModel mdl pln) mdl pln where
    superModel = id

instance HasFrame (SuperModel mdl pln) mdl pln where
    frame f (SuperModel dsn frm) = fmap (\frm' -> SuperModel dsn frm') (f frm)
    {-# INLINE frame #-}

instance HasDesign (SuperModel mdl pln) mdl pln where
    design f (SuperModel dsn frm) = fmap (\dsn' -> SuperModel dsn' frm) (f dsn)
    {-# INLINE design #-}

instance HasPlan (SuperModel mdl pln) pln where
    plan = design . plan

instance HasModel (SuperModel mdl pln) mdl where
    model = design . model
