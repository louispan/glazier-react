{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Common where

import Control.Concurrent.MVar
import Control.Monad.Context
import Data.Tagged.Extras
import qualified GHCJS.Types as J
import Glazier.ShowIO
import System.Mem.Weak

type ShowIOJS = ShowIO J.JSString

type LogName = Tagged "LogName" J.JSString

data RerenderRequired
    = RerenderNotRequired
    | RerenderRequired
    deriving (Show, Eq)

type AskModelWeakVar s = MonadAsk (Tagged "Model" (Weak (MVar s)))
askModelWeakVar :: AskModelWeakVar s m => m (Weak (MVar s))
askModelWeakVar = (untag' @"Model") <$> askContext

type AskModel s = MonadAsk (Tagged "Model" s)
askModel :: AskModel s m => m s
askModel = (untag' @"Model") <$> askContext
