{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Obj.Internal where

import Control.Concurrent.MVar
import Control.Lens.Misc
import Data.IORef
import qualified GHC.Generics as G
import Glazier.React.Plan
import System.Mem.Weak

----------------------------------------------------------------------------------

data WeakObj s = WeakObj
    { planWeakRef :: Weak (IORef Plan)
    , modelWeakVar :: Weak (MVar s)
    } deriving (G.Generic)

makeLenses_ ''WeakObj

data Obj s = Obj
    { planRef :: IORef Plan
    , modelVar :: MVar s
    , weakObj :: WeakObj s
    } deriving (G.Generic)

makeLenses_ ''Obj
