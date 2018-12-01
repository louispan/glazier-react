-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactId.Internal where

import qualified Data.JSString as J
import qualified JavaScript.Extras as JE

-- This id can also be used as the react @key@
data ReactId = ReactId
    { reactIdName :: J.JSString
    , reactIdNum :: Int
    }
    deriving (Read, Show, Eq, Ord)

reactIdKey :: ReactId -> J.JSString
reactIdKey (ReactId n i) = J.append n . J.cons ':' . J.pack $ show i

reactIdKey' :: ReactId -> JE.JSRep
reactIdKey' = JE.toJSR . reactIdKey

-- mkElementalId :: MonadState Int m => J.JSString -> m ElementalId
-- mkElementalId n = ElementalId <$> mkId n
