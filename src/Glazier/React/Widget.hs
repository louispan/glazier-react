{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Widget where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import qualified Data.DList as D
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier as G
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Shared as R
import qualified JavaScript.Extras as JE

type family ActionOf w where
    ActionOf (G.GadgetT act (R.Shared mdl) m (D.DList cmd)) = act
    ActionOf (Display act pln mdl) = act
    ActionOf (Device act pln cmd mdl) = act
    ActionOf (Widget act ol dtl pln cmd mdl) = act

type family OutlineOf w where
    OutlineOf (Widget act ol dtl pln cmd mdl) = ol

type family DetailOf w where
    DetailOf (Widget act ol dtl pln cmd mdl) = dtl
    DetailOf (BaseModel dtl pln) = dtl
    -- DetailOf (R.Shared (BaseModel dtl pln)) = dtl

type family PlanOf w where
    PlanOf (Display act pln mdl) = pln
    PlanOf (Device act pln cmd mdl) = pln
    PlanOf (Widget act ol dtl pln cmd mdl) = pln
    PlanOf (BaseModel dtl pln) = pln
    -- PlanOf (R.Shared (BaseModel dtl pln)) = pln

type family CommandOf w where
    CommandOf (G.GadgetT act (R.Shared mdl) m (D.DList cmd)) = cmd
    CommandOf (Device act pln cmd mdl) = cmd
    CommandOf (Widget act ol dtl pln cmd mdl) = cmd

type family ModelOf w where
    ModelOf (G.GadgetT act (R.Shared mdl) m (D.DList cmd)) = mdl
    ModelOf (G.WindowT mdl m r) = mdl
    ModelOf (Display act pln mdl) = mdl
    ModelOf (Device act pln cmd mdl) = mdl
    ModelOf (Widget act ol dtl pln cmd mdl) = mdl
    ModelOf (BaseModel dtl pln) = BaseModel dtl pln
    -- ModelOf (R.Shared (BaseModel dtl pln)) = R.Shared (BaseModel dtl pln)

type BaseModelOf w = BaseModel (DetailOf w) (PlanOf w)

-- | This is the type used by Gadgets to update a pure Model and also to put it into a MVar forall
-- rendering by other threads.
type EntityOf w = R.Shared (ModelOf w)

-- | The Entity with a BaseModel
type BaseEntityOf w = R.Shared (BaseModelOf w)

type BaseEntity dtl pln = R.Shared (BaseModel dtl pln)

-----------------------------------------------------------

-- | You can't use type family as a type variable for a data type. The workaround is to use
-- a tag to choose between different type family functions.
data Part = Action' | Outline' | Detail' | Plan' | Command' | Model' | BaseModel' | Entity' | BaseEntity'

type family Widget's (p :: Part) w where
    Widget's 'Action' w = ActionOf w
    Widget's 'Outline' w = OutlineOf w
    Widget's 'Detail' w = DetailOf w
    Widget's 'Plan' w = PlanOf w
    Widget's 'Command' w = CommandOf w
    Widget's 'Model' w = ModelOf w
    Widget's 'BaseModel' w = BaseModelOf w
    Widget's 'Entity' w = EntityOf w
    Widget's 'BaseEntity' w = BaseEntityOf w

-----------------------------------------------------------

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

-- | Given an empty MVar, make the Plan that uses the MVar for rendering
class MkRenderingPlan w where
    mkRenderingPlan :: w -> MVar (ModelOf w) -> F (R.Maker (ActionOf w)) (PlanOf w)

class MkPlan w where
    mkPlan :: w -> F (R.Maker (ActionOf w)) (PlanOf w)

-- | Given a deserializable 'Outline', create the 'Detail'
class MkDetail w where
    mkDetail :: w -> OutlineOf w -> F (R.Maker (ActionOf w)) (DetailOf w)

-- | Convert to the serializable outline for saving and restoring
-- This uses a widget parameter because parent widgets that contain other widgets (eg List widget)
-- requires 'HasDetail' which uses a widget parameter.
class ToOutline w where
    outline :: w -> DetailOf w -> OutlineOf w

-- | Lens to the callbacks and interactions with React
-- This class uses a different signature to the standard lens HasXXX typeclasses.
-- This uses a widget argument so that the lens can be explicitly
-- passed into a Widget constructor instead of relying on typeclass inference.
-- This is to avoid overlapping instances.
-- Eg.
-- data Plan1 = Plan1 ...
-- data Plan2 = Plan2 Plan1 ... -- Plan2 contains Plan1
-- class HasPlan1 c Plan1 where plan1 :: Lens c Plan1
-- instance HasPlan1 Plan1
-- class HasPlan2 c Plan2 where plan2 :: Lens c Plan2
-- instance HasPlan1 Plan2 ...
-- gadget1 :: HasPlan1 mdl => Gadget a mdl c
-- gadget2 :: HasPlan2 mdl => Gadget a mdl c
-- The implementation of gadget2 cannot use gadget1 because even though we
-- know that there is a way to convert mdl to Plan2 and then to Plan1
-- the typechecker doesn't know that.
class HasPlan w where
    plan :: w -> Lens' (ModelOf w) (PlanOf w)

-- | Lens to the data for state and rendering.
-- See the notes for 'HasPlan' for why this uses a widget parameter.
class HasDetail w where
    detail :: w -> Lens' (ModelOf w) (DetailOf w)

class IsWindow w where
    -- | Rendering function that uses the BaseModel of Model and Plan.
    window :: w -> G.WindowT (ModelOf w) R.ReactMl ()

class IsGadget w where
    gadget :: w -> G.Gadget (ActionOf w) (R.Shared (ModelOf w)) (D.DList (CommandOf w))

-- | These contain the HTMl attributes that need to be added to the root React element
-- of the initial 'window' and subsequent 'render' functions in order for the
-- 'Device' to work.
class InjectAttributes w where
    windowAttributes :: w -> (ModelOf w) -> WindowAttributes

-- | All Plan should be Disposable
type MkRenderingPlan' w = (MkRenderingPlan w, CD.Disposing (PlanOf w), HasPlan w)

-- | All Plan should be Disposable
type MkPlan' w = (MkPlan w, CD.Disposing (PlanOf w), HasPlan w)

-- | All Details should be Disposable, and convertible ToOutline
type MkDetail' w = (MkDetail w, CD.Disposing (DetailOf w), ToOutline w, HasDetail w)

class (MkRenderingPlan' w, IsWindow w) => IsDisplay w

class (MkPlan' w, IsGadget w, InjectAttributes w) => IsDevice w

-- | NB. A Widget is not a 'InjectAttributes' since it is self contained.
class (MkDetail' w, MkRenderingPlan' w, IsWindow w, IsGadget w) => IsWidget w

------------------------------------------------

-- | A record of Detail and Plan
data BaseModel dtl pln = BaseModel
    { _detail :: dtl
    , _plan :: pln
    } deriving (G.Generic)

instance HasDetail (BaseModel dtl pln) where
    detail _ f (BaseModel dtl pln) = fmap (\dtl' -> BaseModel dtl' pln) (f dtl)
    {-# INLINE detail #-}

instance HasPlan (BaseModel dtl pln) where
    plan _ f (BaseModel dtl pln) = fmap (\pln' -> BaseModel dtl pln') (f pln)
    {-# INLINE plan #-}

instance (CD.Disposing pln, CD.Disposing dtl) => CD.Disposing (BaseModel dtl pln)

-- instance HasDetail (R.Shared (BaseModel dtl pln)) where
--     detail s = R.ival . detail bm
--       where bm = view R.ival s
--     {-# INLINE detail #-}

-- instance HasPlan (R.Shared (BaseModel dtl pln)) where
--     plan s = R.ival . plan bm
--       where bm = view R.ival s
--     {-# INLINE plan #-}

------------------------------------------------

-- | Something that can start the React rendering (eg of the shim component)
-- It has 'mkRenderingPlan' for creating the React render handler which calls
-- the 'window' function
-- Using GADTs to ensure that all constructed Display are instances of IsDisplay.
data Display act pln mdl where
    Display :: (CD.Disposing pln)
        => Lens' mdl pln
        -> (MVar mdl -> F (R.Maker act) pln)
        -> G.WindowT mdl R.ReactMl ()
        -> Display act pln mdl

instance CD.Disposing pln => IsDisplay (Display act pln mdl)

instance HasPlan (Display act pln mdl) where
    plan (Display f _ _) = f

instance MkRenderingPlan (Display act pln mdl) where
    mkRenderingPlan (Display _ f _) = f

instance IsWindow (Display act pln mdl) where
    window (Display _ _ f) = f

------------------------------------------------

instance IsGadget (G.Gadget a (R.Shared mdl) (D.DList c)) where
    gadget = id

------------------------------------------------

instance IsWindow (G.WindowT mdl R.ReactMl ()) where
    window = id

------------------------------------------------

-- | A device is something that has update logic but not rendering logic.
-- Using GADTs to ensure that all constructed Devices are instances of IsDevice.
data Device act pln cmd mdl where
    Device :: CD.Disposing pln
        => Lens' mdl pln
        -> F (R.Maker act) pln
        -> G.Gadget act (R.Shared mdl) (D.DList cmd)
        -> (mdl -> WindowAttributes)
        -> Device act pln cmd mdl

instance CD.Disposing pln => IsDevice (Device act pln cmd mdl)

instance HasPlan (Device act pln cmd mdl) where
    plan (Device f _ _ _) = f

instance MkPlan (Device act pln cmd mdl) where
    mkPlan (Device _ f _ _) = f

instance IsGadget (Device act pln cmd mdl) where
    gadget (Device _ _ f _) = f

instance InjectAttributes (Device act pln cmd mdl) where
    windowAttributes (Device _ _ _ f) = f

------------------------------------------------

-- | Record of functions for a widget. Contains everything you need to make the model,
-- render, and run the event processing.
-- Using GADTs to ensure that all constructed Widgets are instances of IsWidget.
data Widget act ol dtl pln cmd mdl where
    Widget :: (CD.Disposing pln, CD.Disposing dtl)
        => Lens' mdl dtl
        -> Lens' mdl pln
        -> (dtl -> ol)
        -> (ol -> F (R.Maker act) dtl)
        -> (MVar mdl -> F (R.Maker act) pln)
        -> G.WindowT mdl R.ReactMl ()
        -> G.Gadget act (R.Shared mdl) (D.DList cmd)
        -> Widget act ol dtl pln cmd mdl

instance (CD.Disposing pln, CD.Disposing dtl) => IsWidget (Widget act ol dtl pln cmd mdl)

instance HasDetail (Widget act ol dtl pln cmd mdl) where
    detail (Widget f _ _ _ _ _ _) = f

instance HasPlan (Widget act ol dtl pln cmd mdl) where
    plan (Widget _ f _ _ _ _ _) = f

instance ToOutline (Widget act ol dtl pln cmd mdl) where
    outline (Widget _ _ f _ _ _ _) = f

instance MkDetail (Widget act ol dtl pln cmd mdl) where
    mkDetail (Widget _ _ _ f _ _ _) = f

instance MkRenderingPlan (Widget act ol dtl pln cmd mdl) where
    mkRenderingPlan (Widget _ _ _ _ f _ _) = f

instance IsWindow (Widget act ol dtl pln cmd mdl) where
    window (Widget _ _ _ _ _ f _) = f

instance IsGadget (Widget act ol dtl pln cmd mdl) where
    gadget (Widget _ _ _ _ _ _ f) = f

------------------------------------------------

-- | Make a BaseEntity given the Detail, where the Model type is
-- a basic tuple of Detail and Plan.
mkBaseEntity :: (ModelOf w ~ BaseModelOf w, MkRenderingPlan w) => w -> DetailOf w -> F (R.Maker (ActionOf w)) (BaseEntityOf w)
mkBaseEntity w dtl = do
    frm <- R.mkEmptyFrame
    mdl <- BaseModel dtl <$> mkRenderingPlan w frm
    R.putFrame frm mdl
    pure (R.Shared frm mdl)

-- | Make the required Frame and Plan from an Outline
mkBaseEntity' ::
  (ModelOf w ~ BaseModelOf w, MkDetail w, MkRenderingPlan w) =>
  w
  -> OutlineOf w
  -> F (R.Maker (ActionOf w)) (BaseEntityOf w)
mkBaseEntity' w i = mkDetail w i >>= mkBaseEntity w

toDetail :: HasDetail w => w -> ModelOf w -> DetailOf w
toDetail w = view (detail w)

toPlan :: HasPlan w => w -> ModelOf w -> PlanOf w
toPlan w = view (plan w)

toOutline :: ToOutline w => w -> DetailOf w -> OutlineOf w
toOutline w = (outline w)

toOutline' :: (ToOutline w, HasDetail w) => w -> ModelOf w -> OutlineOf w
toOutline' w mdl = (outline w) (toDetail w mdl)
