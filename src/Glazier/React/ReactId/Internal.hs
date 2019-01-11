{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactId.Internal where

import qualified Data.Aeson as A
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified JavaScript.Extras as JE
import JavaScript.Extras.Aeson.Instances()

-- | Constructor is protected to ensure that the Int is uniquely generated
newtype ReactId = ReactId (J.JSString, Int)
    deriving (G.Generic, Read, Show, Eq, Ord)

instance A.ToJSON ReactId where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON ReactId

instance A.ToJSONKey ReactId

instance A.FromJSONKey ReactId

-- This id can also be used as the react @key@
reactIdKey :: ReactId -> J.JSString
reactIdKey (ReactId (n, i)) = J.append n . J.cons ':' . J.pack $ show i

reactIdKey' :: ReactId -> JE.JSRep
reactIdKey' = JE.toJSR . reactIdKey
