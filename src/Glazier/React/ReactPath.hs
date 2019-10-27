{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.ReactPath where

import Control.Monad.Environ
import qualified Data.JSString as J
import qualified GHC.Generics as G
import JavaScript.Extras.Aeson.Instances ()

-- data ReactPathFocus = NewReactPath | CurrentReactPath
--     deriving (G.Generic, Read, Show, Eq, Ord)

-- 'Contains the current react path. The Int is zero indexed.
newtype ReactPath = ReactPath (Maybe (J.JSString, Int), [(J.JSString, Int)])
    deriving (G.Generic, Read, Show, Eq, Ord)

-- instance A.ToJSON ReactPath where
--     toEncoding = A.genericToEncoding A.defaultOptions

-- instance A.FromJSON ReactPath

-- instance A.ToJSONKey ReactPath

-- instance A.FromJSONKey ReactPath

getReactPath :: ReactPath -> [(J.JSString, Int)]
getReactPath (ReactPath (Nothing, xs)) = xs
getReactPath (ReactPath (Just x, xs)) = x : xs

type AskReactPath = MonadAsk' ReactPath
askReactPath :: AskReactPath m => m ReactPath
askReactPath = askEnviron @ReactPath Proxy
localReactPath :: AskReactPath m => (ReactPath -> ReactPath) -> m a -> m a
localReactPath = localEnviron @ReactPath Proxy

type PutReactPath = MonadPut' ReactPath
putReactPath :: PutReactPath m => ReactPath -> m ()
putReactPath = putEnviron @ReactPath Proxy

modifyReactPath :: PutReactPath m => (ReactPath -> ReactPath) -> m ()
modifyReactPath = modifyEnviron @ReactPath Proxy

-- | create a sibling 'ReactPath'
nextReactPath :: J.JSString -> ReactPath -> ReactPath
nextReactPath n (ReactPath (Nothing, ns)) = ReactPath (Just (n, 0), ns)
nextReactPath n (ReactPath (Just (_, i), ns)) = ReactPath (Just (n, i + 1), ns)

-- putNextReactPath :: PutReactPath m => J.JSString -> m ()
-- putNextReactPath n = modifyReactPath $ nextReactPath n

-- | create a child 'ReactPath'
pushReactPath :: ReactPath -> ReactPath
pushReactPath (ReactPath (Nothing, xs)) = ReactPath (Nothing, xs)
pushReactPath (ReactPath (Just x, xs)) = ReactPath (Nothing, x : xs)

-- putPushReactPath :: PutReactPath m => m ()
-- putPushReactPath = modifyReactPath pushReactPath

-- | finish this layer of 'ReactPath'
popReactPath :: ReactPath -> ReactPath
popReactPath (ReactPath (Nothing, [])) = ReactPath (Nothing, [])
popReactPath (ReactPath (Just _, [])) = ReactPath (Nothing, [])
popReactPath (ReactPath (_, x : xs)) = ReactPath (Just x, xs)

-- putPopReactPath :: PutReactPath m => m ()
-- putPopReactPath = modifyReactPath popReactPath

