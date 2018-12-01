-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactId.Internal where

import qualified Data.JSString as J

-- This id can also be used as the react @key@
data ReactId = ReactId
    { reactIdName :: J.JSString
    , reactIdNum :: Int
    }
    deriving (Read, Show, Eq, Ord)

fullReactId :: ReactId -> J.JSString
fullReactId (ReactId n i) = J.append n . J.cons ':' . J.pack $ show i

-- mkElementalId :: MonadState Int m => J.JSString -> m ElementalId
-- mkElementalId n = ElementalId <$> mkId n
