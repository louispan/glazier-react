{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.ReactPath where

import qualified Data.JSString as J
import Data.String
import qualified GHC.Generics as G
-- import JavaScript.Extras.Aeson.Instances ()

default (J.JSString)

-- data ReactPathFocus = NewReactPath | CurrentReactPath
--     deriving (G.Generic, Read, Show, Eq, Ord)

-- | The Maybe component contains the child-most node (if any)
-- The list component are the descendants
-- The Int is zero indexed.
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
    in J.intercalate "." xs'

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

