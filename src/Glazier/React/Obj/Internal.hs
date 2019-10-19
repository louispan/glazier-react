{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Obj.Internal where

import Control.Concurrent.MVar
import Data.IORef
import qualified GHC.Generics as G
import Glazier.React.Plan
import System.Mem.Weak

----------------------------------------------------------------------------------

data Ref v a = Ref (v a) (Weak (v a)) deriving (G.Generic)

strongRef :: Ref v a -> v a
strongRef (Ref s _) = s

weakRef :: Ref v a -> Weak (v a)
weakRef (Ref _ w) = w

type PlanRef = Ref IORef Plan
type ModelVar s = Ref MVar s

data Obj s = Obj PlanRef (ModelVar s)
    deriving (G.Generic)

modelVar :: Obj s -> ModelVar s
modelVar (Obj _ s) = s

planRef :: Obj s -> PlanRef
planRef (Obj p _) = p
