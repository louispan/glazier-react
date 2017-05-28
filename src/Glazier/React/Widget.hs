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
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified JavaScript.Extras as JE

type family ActionOf w where
    -- ActionOf (G.Gadget act (R.Shared mdl) (D.DList cmd)) = act
    ActionOf (Display act pln mdl) = act
    ActionOf (Device act pln cmd mdl) = act
    ActionOf (Widget act ol dtl pln cmd mdl) = act

type family OutlineOf w where
    OutlineOf (Widget act ol dtl pln cmd mdl) = ol

type family DetailOf w where
    DetailOf (Widget act ol dtl pln cmd mdl) = dtl

type family PlanOf w where
    PlanOf (Display act pln mdl) = pln
    PlanOf (Device act pln cmd mdl) = pln
    PlanOf (Widget act ol dtl pln cmd mdl) = pln

type family CommandOf w where
    -- CommandOf (G.Gadget act (R.Shared mdl) (D.DList cmd)) = cmd
    CommandOf (Device act pln cmd mdl) = cmd
    CommandOf (Widget act ol dtl pln cmd mdl) = cmd

-- type family ModelOf w where
--     ModelOf (Display act pln mdl) = mdl
--     ModelOf (Device act pln cmd mdl) = mdl
--     ModelOf (Widget act dtl pln cmd mdl) = mdl

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
newtype WindowAttributes = WindowAttributes ([JE.Property], [R.Handle])

instance Semigroup WindowAttributes where
    WindowAttributes (props, hdls) <> WindowAttributes (props', hdls') = WindowAttributes (props <> props', hdls <> hdls')

instance Monoid WindowAttributes where
    mempty = WindowAttributes ([], [])
    mappend = (<>)

-- | This is used to attach additional Properties and Handles to the rendered widget.
newtype RenderAttributes = RenderAttributes ([JE.Property], [R.Handle])

instance Semigroup RenderAttributes where
    RenderAttributes (props, hdls) <> RenderAttributes (props', hdls') = RenderAttributes (props <> props', hdls <> hdls')

instance Monoid RenderAttributes where
    mempty = RenderAttributes ([], [])
    mappend = (<>)

------------------------------------------------

class (CD.Disposing (PlanOf w)) => MkPlan w mdl where
    -- | Given an empty MVar, make the Plan that uses the MVar for rendering
    mkPlan :: w -> MVar mdl -> F (R.Maker (ActionOf w)) (PlanOf w)

class (CD.Disposing (DetailOf w)) => MkDetail w ol where
    -- | Given a deserializable 'Outline', create the 'Detail'
    mkDetail :: w -> ol -> F (R.Maker (ActionOf w)) (DetailOf w)

class IsWindow w mdl where
    -- | Rendering function that uses the Design of Model and Plan.
    window :: w -> J.JSVal -> G.WindowT mdl R.ReactMl ()

class IsGadget w mdl where
    gadget :: w -> G.Gadget (ActionOf w) (R.Shared mdl) (D.DList (CommandOf w))

-- | These contain the HTMl attributes that need to be added to the root React element
-- of the initial 'window' and subsequent 'render' functions in order for the
-- 'Device' or 'Widget' to work.
class IsAttributes w mdl where
    windowAttributes :: w -> mdl -> WindowAttributes

type IsDevice w mdl = (MkPlan w mdl, IsGadget w mdl, IsAttributes w mdl)

type IsWidget w ol mdl = (MkDetail w ol, IsWindow w mdl, IsDevice w mdl)

------------------------------------------------

-- | Something that can start the React rendering (eg of the shim component)
-- It has 'mkPlan' for creating the React render handler which calls
-- the 'window' function
-- Using GADTs to ensure that all 'Display' are 'mkPlan' instances
data Display act pln mdl where
    Display :: (CD.Disposing pln)
        => (MVar mdl -> F (R.Maker act) pln)
        -> (J.JSVal -> G.WindowT mdl R.ReactMl ())
        -> Display act pln mdl

instance CD.Disposing pln => MkPlan (Display act pln mdl) mdl where
    mkPlan (Display f _) = f

instance IsWindow (Display act pln mdl) mdl where
    window (Display _ f) = f

------------------------------------------------

-- instance IsGadget (G.Gadget a (R.Shared mdl) (D.DList c)) mdl where
--     gadget = id

------------------------------------------------

-- | A device is something that has update logic but not rendering logic.
-- Using GADTs to ensure that all 'Device' are 'mkPlan' instances
data Device act pln cmd mdl where
    Device :: (CD.Disposing pln)
        => (MVar mdl -> F (R.Maker act) pln)
        -> G.Gadget act (R.Shared mdl) (D.DList cmd)
        -> (mdl -> WindowAttributes)
        -> Device act pln cmd mdl

instance CD.Disposing pln => MkPlan (Device act pln cmd mdl) mdl where
    mkPlan (Device f _ _) = f

instance IsGadget (Device act pln cmd mdl) mdl where
    gadget (Device _ f _) = f

instance IsAttributes (Device act pln cmd mdl) mdl where
    windowAttributes (Device _ _ f) = f

------------------------------------------------

-- | Record of functions for a widget. Contains everything you need to make the model,
-- render, and run the event processing.
data Widget act ol dtl pln cmd mdl where
    Widget :: (CD.Disposing pln, CD.Disposing dtl)
        => (ol -> F (R.Maker act) dtl)
        -> (MVar mdl -> F (R.Maker act) pln)
        -> (J.JSVal -> G.WindowT mdl R.ReactMl ())
        -> G.Gadget act (R.Shared mdl) (D.DList cmd)
        -> (mdl -> WindowAttributes)
        -> Widget act ol dtl pln cmd mdl

instance CD.Disposing dtl => MkDetail (Widget act ol dtl pln cmd mdl) ol where
    mkDetail (Widget f _ _ _ _) = f

instance CD.Disposing pln => MkPlan (Widget act ol dtl pln cmd mdl) mdl where
    mkPlan (Widget _ f _ _ _) = f

instance IsWindow (Widget act ol dtl pln cmd mdl) mdl where
    window (Widget _ _ f _ _) = f

instance IsGadget (Widget act ol dtl pln cmd mdl) mdl where
    gadget (Widget _ _ _ f _) = f

instance IsAttributes (Widget act ol dtl pln cmd mdl) mdl where
    windowAttributes (Widget _ _ _ _ f) = f

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
