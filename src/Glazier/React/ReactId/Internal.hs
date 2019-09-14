{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactId.Internal where

import Control.Monad.Context
import Data.Hashable
import qualified GHC.Generics as G

newtype ReactId = ReactId Int
    deriving (G.Generic, Read, Show, Eq, Ord, Hashable)

type AskReactId = MonadAsk ReactId
askReactId :: AskReactId m => m ReactId
askReactId = askContext
