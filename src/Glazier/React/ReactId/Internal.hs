-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactId.Internal where

import qualified Data.JSString as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

import Data.String

-- type MkId m = MonadState Int m

-- mkId :: MonadState Int m => J.JSString -> m J.JSString
-- mkId n = do
--     i <- get
--     let i' = JE.safeIncrement i
--     put i'
--     pure . J.append n . J.cons ':' . J.pack $ show i'

-- This id can also be used as the react @key@
newtype ReactId = ReactId { unReactId :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, IsString, J.IsJSVal, J.PToJSVal)

-- mkElementalId :: MonadState Int m => J.JSString -> m ElementalId
-- mkElementalId n = ElementalId <$> mkId n
