{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.ReactPath where

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

reactPathStr :: ReactPath -> J.JSString
reactPathStr rp =
    let xs = getReactPath rp
        xs' = (\(n, i) -> n <> "-" <> fromString (show i)) <$> xs
    in fold $ intersperse "." xs

getReactPath :: ReactPath -> [(J.JSString, Int)]
getReactPath (ReactPath (Nothing, xs)) = xs
getReactPath (ReactPath (Just x, xs)) = x : xs

-- | create a sibling 'ReactPath'
nextReactPath :: J.JSString -> ReactPath -> ReactPath
nextReactPath n (ReactPath (Nothing, ns)) = ReactPath (Just (n, 0), ns)
nextReactPath n (ReactPath (Just (_, i), ns)) = ReactPath (Just (n, i + 1), ns)

-- | create a child 'ReactPath'
pushReactPath :: ReactPath -> ReactPath
pushReactPath (ReactPath (Nothing, xs)) = ReactPath (Nothing, xs)
pushReactPath (ReactPath (Just x, xs)) = ReactPath (Nothing, x : xs)

-- | finish this layer of 'ReactPath'
popReactPath :: ReactPath -> ReactPath
popReactPath (ReactPath (Nothing, [])) = ReactPath (Nothing, [])
popReactPath (ReactPath (Just _, [])) = ReactPath (Nothing, [])
popReactPath (ReactPath (_, x : xs)) = ReactPath (Just x, xs)

