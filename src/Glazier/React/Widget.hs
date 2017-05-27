{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Glazier.React.Widget where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Monad.Free.Church
import qualified Data.DList as D
import Data.Semigroup
import qualified Glazier as G
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified JavaScript.Extras as JE

type family ActionOf w where
    -- ActionOf (Widget act ol dtl pln cmd) = act
    ActionOf (Display act pln mdl) = act
    ActionOf (G.Gadget act (R.Shared mdl) (D.DList cmd)) = act
    ActionOf (Device act pln cmd mdl) = act

-- type family OutlineOf w where
--     OutlineOf (Device act pln mdl ol cmd) = ol
--     -- OutlineOf (Widget act ol dtl pln cmd) = ol

-- type family DetailOf w where
--     DetailOf (Device act pln mdl cmd) = dtl

type family PlanOf w where
    -- PlanOf (Widget act ol dtl pln cmd) = pln
    PlanOf (Display act pln mdl) = pln
    PlanOf (Device act pln cmd mdl) = pln

type family CommandOf w where
    CommandOf (G.Gadget act (R.Shared mdl) (D.DList cmd)) = cmd
    CommandOf (Device act pln cmd mdl) = cmd
--     CommandOf (Widget act ol dtl pln cmd) = cmd

type family ModelOf w where
    ModelOf (Display act pln mdl) = mdl
    ModelOf (Device act pln cmd mdl) = mdl

-- type ModelOf w s p = (R.HasDetail s (PlanOf w), R.HasPlan p (DetailOf w)) => R.Model s p

-- type FrameOf w s p = (R.HasDetail s (PlanOf w), R.HasPlan p (DetailOf w)) => R.Frame s p

-- type EntityOf w s p = (R.HasDetail s (PlanOf w), R.HasPlan p (DetailOf w)) => R.Entity s p

-- type WindowOf w s p = (R.HasDetail s (PlanOf w), R.HasPlan p (DetailOf w)) => G.WindowT (R.Model s p) R.ReactMl ()

-- type GadgetOf w s p = (R.HasDetail s (PlanOf w), R.HasPlan p (DetailOf w)) => G.Gadget (ActionOf w) (R.Entity s p) (D.DList (CommandOf w))

------------------------------------------------

-- -- | You can't use type family as a type variable for a data type. The workaround is to use
-- -- a tag to choose between different type family functions.
-- type family Widget's tag w where
--     Widget's EntityType w = EntityOf w
--     Widget's OutlineType w = OutlineOf w

-- -- | tag used to choose EntityOf type function. See @Widget's@.
-- data EntityType
-- -- | tag used to choose OutlineOf type funciton. See @Widget's@.
-- data OutlineType

------------------------------------------------

-- | This is used to attach additional Properties and Handles to the shim component.
newtype WindowAttrs = WindowAttrs ([JE.Property], [R.Handle])

instance Semigroup WindowAttrs where
    WindowAttrs (props, hdls) <> WindowAttrs (props', hdls') = WindowAttrs (props <> props', hdls <> hdls')

instance Monoid WindowAttrs where
    mempty = WindowAttrs ([], [])
    mappend = (<>)

-- | This is used to attach additional Properties and Handles to the rendered widget.
newtype RenderAttrs = RenderAttrs ([JE.Property], [R.Handle])

instance Semigroup RenderAttrs where
    RenderAttrs (props, hdls) <> RenderAttrs (props', hdls') = RenderAttrs (props <> props', hdls <> hdls')

instance Monoid RenderAttrs where
    mempty = RenderAttrs ([], [])
    mappend = (<>)

------------------------------------------------

class (CD.Disposing (PlanOf w)) => MkPlan w mdl where
    -- | Given an empty MVar, make the Plan that uses the MVar for rendering
    mkPlan :: w -> MVar mdl -> F (R.Maker (ActionOf w)) (PlanOf w)

-- class (CD.Disposing (DetailOf w)) => MkDetail w mdl ol where
--     -- | Given a deserializable 'Outline', create the 'Detail'
--     mkDetail :: w -> ol -> F (R.Maker (ActionOf w)) (DetailOf w)

class IsWindow w mdl where
    -- | Rendering function that uses the Design of Model and Plan.
    window :: w -> G.WindowT mdl R.ReactMl ()

class IsGadget w mdl where
    gadget :: w -> G.Gadget (ActionOf w) (R.Shared mdl) (D.DList (CommandOf w))

class (MkPlan w mdl, IsGadget w mdl) => IsDevice w mdl where
    windowAttrs :: w -> mdl -> WindowAttrs
    renderAttrs :: w -> mdl -> RenderAttrs

-- type IsWidget w = (IsDisplay w, IsGizmo w)

------------------------------------------------

-- | Using GADTs to ensure that all 'Display' are 'mkPlan' instances
data Display act pln mdl where
    Display :: (CD.Disposing pln)
        => (MVar mdl -> F (R.Maker act) pln)
        -> G.WindowT mdl R.ReactMl ()
        -> Display act pln mdl

instance CD.Disposing pln => MkPlan (Display act pln mdl) mdl where
    mkPlan (Display f _) = f

instance IsWindow (Display act pln mdl) mdl where
    window (Display _ f) = f

------------------------------------------------

instance IsGadget (G.Gadget a (R.Shared mdl) (D.DList c)) mdl where
    gadget = id

------------------------------------------------

-- | Using GADTs to ensure that all 'Device' are 'mkPlan' instances
data Device act pln cmd mdl where
    Device :: (CD.Disposing pln)
        => (MVar mdl -> F (R.Maker act) pln)
        -> G.Gadget act (R.Shared mdl) (D.DList cmd)
        -> (mdl -> WindowAttrs)
        -> (mdl -> RenderAttrs)
        -> Device act pln cmd mdl

instance CD.Disposing pln => MkPlan (Device act pln cmd mdl) mdl where
    mkPlan (Device f _ _ _) = f

instance IsGadget (Device act pln cmd mdl) mdl where
    gadget (Device _ f _ _) = f

instance CD.Disposing pln => IsDevice (Device act pln cmd mdl) mdl where
    windowAttrs (Device _ _ f _) = f
    renderAttrs (Device _ _ _ f) = f

-- | Record of functions for a widget. Contains everything you need to make the model,
-- render, and run the event processing.
-- This is a GADT to enforce the Disposing and ToOutline constraints at the time
-- of creating the Widget record, which ensures all values of Widget is a valid IsWidget instance.
-- data Widget a o s p c where
--     Widget :: (CD.Disposing s, CD.Disposing p, R.ToOutline s o)
--         => (o -> F (R.Maker a) s)
--         -- | HasModel allows using the mkPlan in composite widgets
--         -- so that larger models can be used to initialize the window function below.
--         -> (forall mdl. (R.HasModel mdl s p) => MVar mdl -> F (R.Maker a) p)
--         -- | HasModel allows using the window in composite widgets
--         -- so that the window can use data from larger models
--         -- (eg. to add to class properties of html elements).
--         -> (forall mdl. R.HasModel mdl s p => G.WindowT mdl R.ReactMl ())
--         -- | Gadget doesn't use HasEntity as it can be easily zoomed.
--         -> G.Gadget a (R.Entity m p) (D.DList c)
--         -> Widget a o s p c

-- instance (CD.Disposing s, R.ToOutline s o) =>
--          MkModel (Widget a o s p c) where
--     mkModel (Widget f _ _ _) = f

-- instance (CD.Disposing s, CD.Disposing p) =>
--          MkPlan (Widget a o s p c) where
--     mkPlan (Widget _ f _ _) = f

-- instance (CD.Disposing s, CD.Disposing p) =>
--          IsWindow (Widget a o s p c) where
--     window (Widget _ _ f _) = f

-- instance (CD.Disposing m, CD.Disposing p, R.ToOutline m o) =>
--          IsWidget (Widget a o m p c) where
--     mkModel (Widget f _ _ _) = f
--     mkPlan (Widget _ f _ _) = f
--     window (Widget _ _ f _) = f
--     gadget (Widget _ _ _ f) = f


-------------------------------------------------------------

-- -- | Make the required Frame and Plan for a Model
-- mkEntity :: IsWidget w => w -> DetailOf w -> F (R.Maker (ActionOf w)) (R.Entity (DetailOf w) (PlanOf w))
-- mkEntity w dtl = do
--     frm <- R.mkEmptyFrame
--     mdl <- R.Model dtl <$> mkPlan w frm
--     R.putFrame frm mdl
--     pure (R.Entity mdl frm)

-- -- | Make the required Frame and Plan from an Outline
-- mkEntity' ::
--   IsWidget w =>
--   w
--   -> OutlineOf w
--   -> F (R.Maker (ActionOf w)) (R.Entity (DetailOf w) (PlanOf w))
-- mkEntity' w i = mkModel w i >>= mkEntity w

-- Detail
-- Plan
-- Design
-- Object
-- Model contains object
-- Concept
-- Project
-- Recipe
-- Formula
-- Modify
-- Register
-- Label
