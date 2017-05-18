{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget where

import qualified Control.Disposable as CD
import Control.Monad.Free.Church
import qualified Data.DList as D
import qualified Glazier as G
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R

type family ActionOf w where
    ActionOf (Widget a e o m p c) = a

type family ExceptionOf w where
    ExceptionOf (Widget a e o m p c) = e

type family OutlineOf w where
    OutlineOf (Widget a e o m p c) = o

type family ModelOf w where
    ModelOf (Widget a e o m p c) = m

type family PlanOf w where
    PlanOf (Widget a e o m p c) = p

type family CommandOf w where
    CommandOf (Widget a e o m p c) = c

type SceneOf w = R.Scene (ModelOf w) (PlanOf w)

type FrameOf w = R.Frame (ModelOf w) (PlanOf w)

type GizmoOf w = R.Gizmo (ModelOf w) (PlanOf w)

type WindowOf w = G.WindowT (SceneOf w) R.ReactMl ()

type GadgetOf w = G.Gadget (ActionOf w) (ExceptionOf w) (GizmoOf w) (D.DList (CommandOf w))

-- | tag used to choose GizmoOf type function. See @Widget's@.
data GizmoType
-- | tag used to choose OutlineOf type funciton. See @Widget's@.
data OutlineType

-- | You can't use type family as a type variable for a data type. The workaround is to use
-- a tag to choose between different type family functions.
type family Widget's tag w where
    Widget's GizmoType w = GizmoOf w
    Widget's OutlineType w = OutlineOf w

-- | Record of functions for a widget. Contains everything you need to make the model,
-- render, and run the event processing.
-- This is a GADT to enforce the Disposing and ToOutline constraints at the time
-- of creating the Widget record, which ensures all values of Widget is a valid IsWidget instance.
data Widget a e o m p c where
    Widget
         :: (CD.Disposing m, CD.Disposing p, R.ToOutline m o)
         => (o -> F (R.Maker a) m)
         -> (R.Frame m p -> F (R.Maker a) p)
         -> G.WindowT (R.Scene m p) R.ReactMl ()
         -> G.Gadget a e (R.Gizmo m p) (D.DList c)
         -> Widget a e o m p c

-- | This typeclass is convenient as it carries the 'Disposing Model' and 'Disposing Plan' constraints
-- and allows treating 'Widget a e o m p c' as a type 'w'
class (CD.Disposing (ModelOf w)
      , CD.Disposing (PlanOf w)
      , R.ToOutline (ModelOf w) (OutlineOf w)) => IsWidget w where
    -- | Make a Model from an Outline
    mkModel :: w -> OutlineOf w -> F (R.Maker (ActionOf w)) (ModelOf w)
    -- | Given an empty frame, make the Plan that uses the frame for rendering
    mkPlan :: w -> R.Frame (ModelOf w) (PlanOf w) -> F (R.Maker (ActionOf w)) (PlanOf w)
    -- | Rendering function that uses the Scene of Model and Plan
    window :: w -> G.WindowT (R.Scene (ModelOf w) (PlanOf w)) R.ReactMl ()
    -- | Update function that processes Action to update the Frame and Scene
    gadget :: w -> G.Gadget (ActionOf w) (ExceptionOf w)(R.Gizmo (ModelOf w) (PlanOf w)) (D.DList (CommandOf w))

instance (CD.Disposing m, CD.Disposing p, R.ToOutline m o) => IsWidget (Widget a e o m p c) where
    mkModel (Widget f _ _ _) = f
    mkPlan (Widget _ f _ _) = f
    window (Widget _ _ f _) = f
    gadget (Widget _ _ _ f) = f

-- | Make the required Frame and Plan for a Model
mkGizmo :: IsWidget w => w -> ModelOf w -> F (R.Maker (ActionOf w)) (R.Gizmo (ModelOf w) (PlanOf w))
mkGizmo w mdl = do
    frm <- R.mkEmptyFrame
    scn <- R.Scene mdl <$> mkPlan w frm
    R.putFrame frm scn
    pure (R.Gizmo scn frm)

-- | Make the required Frame and Plan from an Outline
mkGizmo' ::
  IsWidget w =>
  w
  -> OutlineOf w
  -> F (R.Maker (ActionOf w)) (R.Gizmo (ModelOf w) (PlanOf w))
mkGizmo' w i = mkModel w i >>= mkGizmo w
