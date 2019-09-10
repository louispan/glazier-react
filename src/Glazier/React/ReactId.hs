{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.ReactId where

import Control.Monad.Context
import Control.Monad.Reader
import Data.String
import qualified Data.JSString as J
import qualified GHC.Generics as G
import Glazier.Logger
import JavaScript.Extras.Aeson.Instances ()

data ReactIdStep = NewReactId | CurrentReactId
    deriving (G.Generic, Read, Show, Eq, Ord)

-- 'Contains the current react id. The Int is zero indexed.
data ReactId = ReactId ReactIdStep [(J.JSString, Int)]
    deriving (G.Generic, Read, Show, Eq, Ord)

-- instance A.ToJSON ReactId where
--     toEncoding = A.genericToEncoding A.defaultOptions

-- instance A.FromJSON ReactId

-- instance A.ToJSONKey ReactId

-- instance A.FromJSONKey ReactId

type AskReactId = MonadAsk ReactId
askReactId :: AskReactId m => m ReactId
askReactId = askContext

-- | We can get the LogName from a ReactId
type AskLogNameJS = AskLogName J.JSString
instance {-# OVERLAPPING #-} Monad m => MonadAsk (LogName J.JSString) (ReaderT ReactId m) where
  askContext = do
    ReactId _ ns <- askReactId
    let xs = (\(n, i) -> n <> (fromString $ show i)) <$> ns
    pure . LogName $ foldr (<>) "" xs

type PutReactId = MonadPut ReactId
putReactId :: PutReactId m => ReactId -> m ()
putReactId = putContext

modifyReactId :: PutReactId m => (ReactId -> ReactId) -> m ()
modifyReactId = modifyContext

-- | create a sibling 'ReactId'
nextReactId :: J.JSString -> ReactId -> ReactId
nextReactId n (ReactId NewReactId ns) = ReactId CurrentReactId ((n, 0) : ns)
nextReactId n (ReactId CurrentReactId []) = ReactId CurrentReactId [(n, 0)]
nextReactId n (ReactId CurrentReactId ((_, i) : ns)) = ReactId CurrentReactId ((n, i + 1) : ns)

putNextReactId :: PutReactId m => J.JSString -> m ()
putNextReactId n = modifyReactId $ nextReactId n

-- | create a child 'ReactId'
pushReactId :: ReactId -> ReactId
pushReactId (ReactId _ ns) = ReactId NewReactId ns

putPushReactId :: PutReactId m => m ()
putPushReactId = modifyReactId $ pushReactId

-- | finish this layer of 'ReactId'
popReactId :: ReactId -> ReactId
popReactId (ReactId _ []) = ReactId NewReactId []
popReactId (ReactId NewReactId ns) = ReactId CurrentReactId ns
popReactId (ReactId CurrentReactId (_ : ns)) = ReactId CurrentReactId ns

putPopReactId :: PutReactId m => m ()
putPopReactId = modifyReactId popReactId
