{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Obj.Internal where

import Control.Lens.Misc
import qualified GHC.Generics as G
import Glazier.React.Plan
import Glazier.React.Common

----------------------------------------------------------------------------------

data WeakObj s = WeakObj
    { planWeakRef :: WeakRef Plan
    , modelWeakRef :: WeakRef s
    } deriving (G.Generic)

makeLenses_ ''WeakObj

data Obj s = Obj
    { planRef :: Ref Plan
    , modelRef :: Ref s
    , weakObj :: WeakObj s
    } deriving (G.Generic)

makeLenses_ ''Obj
