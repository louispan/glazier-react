{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactId.Internal where

import qualified Data.JSString as J
import qualified JavaScript.Extras as JE

-- | Constructor is protected to ensure that the Int is uniquely generated
newtype ReactId = ReactId (J.JSString, Int)
    deriving (Read, Show, Eq, Ord)

-- This id can also be used as the react @key@
reactIdKey :: ReactId -> J.JSString
reactIdKey (ReactId (n, i)) = J.append n . J.cons ':' . J.pack $ show i

reactIdKey' :: ReactId -> JE.JSRep
reactIdKey' = JE.toJSR . reactIdKey
