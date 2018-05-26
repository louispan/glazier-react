{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.MkId.Internal where

import Control.Monad.State
import qualified Data.JSString as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

import Data.String
-- | A monad that can safely create Widgets with unique
-- 'GadgetId' and 'PlanId'
-- prototype :: MonadState Int p => p (Widget w x s m c)
-- protot

type MkId m = MonadState Int m

mkId :: MonadState Int m => J.JSString -> m J.JSString
mkId n = do
    i <- get
    let i' = JE.safeIncrement i
    put i'
    pure . J.append n . J.cons ':' . J.pack $ show i'

-- the base monad bind:
-- returns a STM async
-- which actually creates a STM Ref to fire once-off results into
-- for the pure thing to read?

-- This id can also be used as the react @key@
newtype ElementalId = ElementalId { unElementalId :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, IsString, J.IsJSVal, J.PToJSVal)

newtype PlanId = PlanId { unPlanId :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, IsString, J.IsJSVal, J.PToJSVal)

mkElementalId :: MonadState Int m => J.JSString -> m ElementalId
mkElementalId n = ElementalId <$> mkId n

mkPlanId :: MonadState Int m => J.JSString -> m PlanId
mkPlanId n = PlanId <$> mkId n
