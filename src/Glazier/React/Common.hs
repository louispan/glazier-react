{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Common where

import Control.Monad.Context
import Data.IORef
import Data.Tagged.Extras
import qualified GHCJS.Types as J
import Glazier.Logger
import Glazier.ShowIO
import System.Mem.Weak

type Ref s = IORef s
type WeakRef s = Weak (IORef s)

type ShowIOJS = ShowIO J.JSString

type LoggerJS c m = Logger J.JSString c m
type LogNameJS = LogName J.JSString

data RerenderRequired
    = RerenderNotRequired
    | RerenderRequired
    deriving (Show, Eq)

-- | Avoids ambiguous types for 'askModel' and 'askModelWeakRef'
-- newtype Model s = Model { getModel :: s }
type AskModelWeakRef s = MonadAsk (Tagged "Model" (WeakRef s))
askModelWeakRef :: AskModelWeakRef s m => m (WeakRef s)
askModelWeakRef = (untag' @"Model") <$> askContext

type AskModel s = MonadAsk (Tagged "Model" s)
askModel :: AskModel s m => m s
askModel = (untag' @"Model") <$> askContext
