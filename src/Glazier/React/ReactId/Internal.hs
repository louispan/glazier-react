{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactId.Internal where

import Data.Hashable
import qualified GHC.Generics as G

newtype ReactId = ReactId Int
    deriving (G.Generic, Read, Show, Eq, Ord, Hashable)

unReactId :: ReactId -> Int
unReactId (ReactId i) = i
