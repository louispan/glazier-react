{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import qualified Data.DList as D
import qualified Glazier as G
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R

type family CommandOf w where
    CommandOf (Widget c a m p) = c

type family ActionOf w where
    ActionOf (Widget c a m p) = a

type family ModelOf w where
    ModelOf (Widget c a m p) = m

type family PlanOf w where
    PlanOf (Widget c a m p) = p

-- | Undecidable instances! But this is safe because 'ModelOf w' and 'PlanOf w'
-- is guaranteed to be smaller than closed type family 'DesignOf w'.
type DesignOf w = R.Design (ModelOf w) (PlanOf w)

type FrameOf w = R.Frame (ModelOf w) (PlanOf w)

type SuperModelOf w = R.SuperModel (ModelOf w) (PlanOf w)

-- | Record of functions for a widget. Contains everything you need to make the model,
-- render, and run the event processing.
data Widget c a m p = Widget
    (R.Frame m p -> F (R.Maker a) p)
    (G.WindowT (R.Design m p) (R.ReactMlT Identity) ())
    (G.GadgetT a (R.SuperModel m p) Identity (D.DList c))

-- | This typeclass is convenient as it carries the 'Disposing Model' and 'Disposing Plan' constraints
-- and allows treating 'Widget c a m p' as a type 'w'
class (CD.Disposing (ModelOf w)
      , CD.Disposing (PlanOf w)) => IsWidget w where
    -- | Given an empty frame, make the Plan that uses the frame for rendering
    mkPlan :: w -> R.Frame (ModelOf w) (PlanOf w) -> F (R.Maker (ActionOf w)) (PlanOf w)
    -- | Rendering function that uses the Design of Model and Plan
    window :: w -> G.WindowT (R.Design (ModelOf w) (PlanOf w)) (R.ReactMlT Identity) ()
    -- | Update function
    gadget :: w -> G.GadgetT (ActionOf w) (R.SuperModel (ModelOf w) (PlanOf w)) Identity (D.DList (CommandOf w))

instance (CD.Disposing m, CD.Disposing p) => IsWidget (Widget c a m p) where
    mkPlan (Widget f _ _) = f
    window (Widget _ f _) = f
    gadget (Widget _ _ f) = f

mkSuperModel :: IsWidget w => w -> ModelOf w -> F (R.Maker (ActionOf w)) (R.SuperModel (ModelOf w) (PlanOf w))
mkSuperModel w mdl = do
    frm <- R.mkEmptyFrame
    dsn <- R.Design mdl <$> mkPlan w frm
    R.putFrame frm dsn
    pure (R.SuperModel dsn frm)
