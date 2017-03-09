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

class HasWidgetGasket c gsk | c -> gsk where
    widgetGasket :: Lens' c gsk

class HasWidgetModel c mdl | c -> mdl where
    widgetModel :: Lens' c mdl

data GModel gsk mdl = GModel
    { _widgetGasket :: gsk
    , _widgetModel :: mdl
    } deriving (G.Generic)

instance (CD.Disposing gsk, CD.Disposing mdl) => CD.Disposing (GModel gsk mdl)

instance HasWidgetGasket (GModel gsk mdl) gsk where
    widgetGasket f (GModel gsk mdl) = fmap (\gsk' -> GModel gsk' mdl) (f gsk)
    {-# INLINE widgetGasket #-}

instance HasWidgetModel (GModel gsk mdl) mdl where
    widgetModel f (GModel gsk mdl) = fmap (\mdl' -> GModel gsk mdl') (f mdl)
    {-# INLINE widgetModel #-}

class HasGModel c gsk mdl | c -> gsk mdl where
    gModel :: Lens' c (GModel gsk mdl)

instance HasGModel (GModel gsk mdl) gsk mdl where
    gModel = id

type MModel gsk mdl = MVar (GModel gsk mdl)

class HasMModel c gsk mdl | c -> gsk mdl where
    mModel :: Lens' c (MModel gsk mdl)

instance HasMModel (MModel gsk mdl) gsk mdl where
    mModel = id

data SuperModel gsk mdl = SuperModel
    { _mModel :: MModel gsk mdl
    , _gModel :: GModel gsk mdl
    } deriving (G.Generic)

instance CD.Disposing (GModel gsk mdl) => CD.Disposing (SuperModel gsk mdl) where
    disposing s = CD.disposing $ s ^. gModel

class (HasMModel c gsk mdl, HasGModel c gsk mdl) => HasSuperModel c gsk mdl | c -> gsk mdl where
    superModel :: Lens' c (SuperModel gsk mdl)

instance HasSuperModel (SuperModel gsk mdl) gsk mdl where
    superModel = id

instance HasMModel (SuperModel gsk mdl) gsk mdl where
    mModel f (SuperModel mm gm) = fmap (\mm' -> SuperModel mm' gm) (f mm)
    {-# INLINE mModel #-}

instance HasGModel (SuperModel gsk mdl) gsk mdl where
    gModel f (SuperModel mm gm) = fmap (\gm' -> SuperModel mm gm') (f gm)
    {-# INLINE gModel #-}

class IsWidget w where
    -- The input to Gadget
    type WidgetAction w :: *

    -- The output of Gadget
    type WidgetCommand w :: *

    -- The pure model for state and rendering
    type WidgetModel w :: *

    -- Callbacks and data required for interfacing with react.
    type WidgetGasket w :: *

type WidgetGModel w = GModel (WidgetGasket w) (WidgetModel w)
type WidgetMModel w = MModel (WidgetGasket w) (WidgetModel w)
type WidgetSuperModel w = SuperModel (WidgetGasket w) (WidgetModel w)
