{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Widget where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import qualified GHC.Generics as G

class HasWidgetPlan c pln | c -> pln where
    widgetPlan :: Lens' c pln

class HasWidgetModel c mdl | c -> mdl where
    widgetModel :: Lens' c mdl

data Design mdl pln = Design
    { _widgetModel :: mdl
    , _widgetPlan :: pln
    } deriving (G.Generic)

instance (CD.Disposing pln, CD.Disposing mdl) => CD.Disposing (Design mdl pln)

instance HasWidgetPlan (Design mdl pln) pln where
    widgetPlan f (Design mdl pln) = fmap (\pln' -> Design mdl pln') (f pln)
    {-# INLINE widgetPlan #-}

instance HasWidgetModel (Design mdl pln) mdl where
    widgetModel f (Design mdl pln) = fmap (\mdl' -> Design mdl' pln) (f mdl)
    {-# INLINE widgetModel #-}

class HasDesign c mdl pln | c -> mdl pln where
    design :: Lens' c (Design mdl pln)

instance HasDesign (Design mdl pln) mdl pln where
    design = id

type Replica mdl pln = MVar (Design mdl pln)

class HasReplica c mdl pln | c -> mdl pln where
    replica :: Lens' c (Replica mdl pln)

instance HasReplica (Replica mdl pln) mdl pln where
    replica = id

data SuperModel mdl pln = SuperModel
    { _design :: Design mdl pln
    , _replica :: Replica mdl pln
    } deriving (G.Generic)

instance CD.Disposing (Design mdl pln) => CD.Disposing (SuperModel mdl pln) where
    disposing s = CD.disposing $ s ^. design

class (HasDesign c mdl pln, HasReplica c mdl pln) => HasSuperModel c mdl pln | c -> mdl pln where
    superModel :: Lens' c (SuperModel mdl pln)

instance HasSuperModel (SuperModel mdl pln) mdl pln where
    superModel = id

instance HasReplica (SuperModel mdl pln) mdl pln where
    replica f (SuperModel dsn rep) = fmap (\rep' -> SuperModel dsn rep') (f rep)
    {-# INLINE replica #-}

instance HasDesign (SuperModel mdl pln) mdl pln where
    design f (SuperModel dsn rep) = fmap (\dsn' -> SuperModel dsn' rep) (f dsn)
    {-# INLINE design #-}

class IsWidget w where
    -- The input to Gadget
    type WidgetAction w :: *

    -- The output of Gadget
    type WidgetCommand w :: *

    -- The pure model for state and rendering
    type WidgetModel w :: *

    -- Callbacks and data required for interfacing with react.
    type WidgetPlan w :: *

type WidgetDesign w = Design (WidgetModel w) (WidgetPlan w)
type WidgetReplica w = Replica (WidgetModel w) (WidgetPlan w)
type WidgetSuperModel w = SuperModel (WidgetModel w) (WidgetPlan w) 
