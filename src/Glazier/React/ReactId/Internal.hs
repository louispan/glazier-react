{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactId.Internal where

import Control.Monad.Context
import Control.Newtype.Generics
import qualified Data.Aeson as A
import qualified Data.JSString as J
import qualified Data.List.NonEmpty as NE
import qualified GHC.Generics as G
import JavaScript.Extras.Aeson.Instances()

newtype ReactId = ReactId { unReactId :: (NE.NonEmpty J.JSString, Int) }
    deriving (G.Generic, Read, Show, Eq, Ord)

instance Newtype ReactId

instance A.ToJSON ReactId where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON ReactId

instance A.ToJSONKey ReactId

instance A.FromJSONKey ReactId

type AskReactId = MonadAsk ReactId
askReactId :: AskReactId m => m ReactId
askReactId = askContext

type PutReactId = MonadPut ReactId
putReactId :: PutReactId m => ReactId -> m ()
putReactId = putContext

modifyReactId :: PutReactId m => (ReactId -> ReactId) -> m ()
modifyReactId = modifyContext
