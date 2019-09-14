{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Obj.Internal where

import Control.Lens.Misc
import Control.Monad.Context
import Data.Tagged.Extras
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
