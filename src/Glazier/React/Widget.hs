{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Widget where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import qualified Data.DList as D
import qualified Glazier as G
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R

class (CD.Disposing (ModelOf w)
      , CD.Disposing (PlanOf w)) => IsWidget w where
    -- | The pure model for state and rendering
    type CommandOf w :: *
    type ActionOf w :: *
    type ModelOf w :: *
    type PlanOf w :: *
    mkPlan :: w -> R.Frame (ModelOf w) (PlanOf w) -> F (R.Maker (ActionOf w)) (PlanOf w)
    window :: w -> G.WindowT (R.Design (ModelOf w) (PlanOf w)) (R.ReactMlT Identity) ()
    gadget :: w -> G.GadgetT (ActionOf w) (R.SuperModel (ModelOf w) (PlanOf w)) Identity (D.DList (CommandOf w))

type family DesignOf w where
    DesignOf w = R.Design (ModelOf w) (PlanOf w)

type family FrameOf w where
    FrameOf w = R.Frame (ModelOf w) (PlanOf w)

type family SuperModelOf w where
    SuperModelOf w = R.SuperModel (ModelOf w) (PlanOf w)

-- | Contains everything you need to make the model,
-- render, and run the event processing.
data Widget c a m p = Widget
    (R.Frame m p -> F (R.Maker a) p)
    (G.WindowT (R.Design m p) (R.ReactMlT Identity) ())
    (G.GadgetT a (R.SuperModel m p) Identity (D.DList c))

instance (CD.Disposing m, CD.Disposing p) =>
         IsWidget (Widget c a m p) where
    type CommandOf (Widget c a m p) = c
    type ActionOf (Widget c a m p) = a
    type ModelOf (Widget c a m p) = m
    type PlanOf (Widget c a m p) = p
    mkPlan (Widget f _ _) = f
    window (Widget _ f _) = f
    gadget (Widget _ _ f) = f

mkSuperModel :: IsWidget w => w -> ModelOf w -> F (R.Maker (ActionOf w)) (R.SuperModel (ModelOf w) (PlanOf w))
mkSuperModel w mdl = do
    frm <- R.mkEmptyFrame
    dsn <- R.Design mdl <$> mkPlan w frm
    R.putFrame frm dsn
    pure (R.SuperModel dsn frm)
