{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Obj.Internal where

import Control.Concurrent.MVar
import Control.Lens
import Data.IORef
import qualified GHC.Generics as G
import Glazier.React.Plan
import System.Mem.Weak

----------------------------------------------------------------------------------

data Ref v a = Ref (v a) (Weak (v a))
    deriving (G.Generic)

strongRef :: Ref v a -> v a
strongRef (Ref s _) = s

_strongRef :: (Profunctor p, Contravariant f) => Optic' p f (Ref v a) (v a)
_strongRef = to strongRef

weakRef :: Ref v a -> Weak (v a)
weakRef (Ref _ w) = w

_weakRef :: (Profunctor p, Contravariant f) => Optic' p f (Ref v a) (Weak (v a))
_weakRef = to weakRef

type PlanRef = Ref IORef Plan
type ModelVar s = Ref MVar s

data Obj s = Obj PlanRef (ModelVar s)
    deriving (G.Generic)

modelVar :: Obj s -> ModelVar s
modelVar (Obj _ s) = s

_modelVar :: (Profunctor p, Contravariant f) => Optic' p f (Obj s) (ModelVar s)
_modelVar = to modelVar

planRef :: Obj s -> PlanRef
planRef (Obj p _) = p

_planRef :: (Profunctor p, Contravariant f) => Optic' p f (Obj s) PlanRef
_planRef = to planRef

data WeakObj s = WeakObj (Weak (IORef Plan)) (Weak (MVar s))
    deriving (G.Generic)

weakObj :: Obj s -> WeakObj s
weakObj (Obj (Ref _ plnWk) (Ref _ mdlWk)) = WeakObj plnWk mdlWk

_weakObj :: (Profunctor p, Contravariant f) => Optic' p f (Obj s) (WeakObj s)
_weakObj = to weakObj

