{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Common where

import Control.Concurrent.MVar
import Control.Monad.Context
import Data.Tagged.Extras
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import System.Mem.Weak

type LogName = Tagged "LogName" J.JSString

-- | A 'Handler' comprises to two functions
-- Firstly, a function that is guaranteed to be "GHCJS synchronous"
-- and not have any black holes that will turn it into an "GHCJS asynchronous" call.
-- See http://hackage.haskell.org/package/ghcjs-base-0.2.0.0/docs/GHCJS-Concurrent.html
-- Secondly, a function that is not guaranteed to be "GHCJS synchronous"
-- and may continue work asynchronously.
-- The reason for this is detailed in 'Glazier.React.Reactor.Exec.execMkHandler'
type Handler = (J.JSVal -> IO (), IO ())

-- A 'Listener' is a callable function from JS that accepts a JS value as in input.
type Listener = J.Callback (J.JSVal -> IO ())

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
