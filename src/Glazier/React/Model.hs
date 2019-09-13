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

module Glazier.React.Model where

import Control.Lens.Misc
import Control.Monad.Context
import Data.Tagged.Extras
import qualified GHC.Generics as G
import Glazier.React.Plan
import Glazier.React.Type

----------------------------------------------------------------------------------

-- | Avoids ambiguous types for 'askModel' and 'askModelWeakRef'
-- newtype Model s = Model { getModel :: s }
type AskModelWeakRef s = MonadAsk (Tagged "Model" (WeakRef s))
askModelWeakRef :: AskModelWeakRef s m => m (WeakRef s)
askModelWeakRef = (untag' @"Model") <$> askContext

type AskModel s = MonadAsk (Tagged "Model" s)
askModel :: AskModel s m => m s
askModel = (untag' @"Model") <$> askContext


data WeakObj s = WeakObj
    { planWeakRef :: WeakRef Plan
    , modelWeakRef :: WeakRef s
    } deriving (G.Generic)

makeLenses_ ''WeakObj

-- FIXME: Protect constructor so that reads cannot be used without registering listeners
data Obj s = Obj
    { planRef :: Ref Plan
    , modelRef :: Ref s
    , weakObj :: WeakObj s
    } deriving (G.Generic)

makeLenses_ ''Obj
