{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Glazier.React.ReactId where

import Control.Monad.Env
import qualified Data.Aeson as A
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified JavaScript.Extras()
import Control.Newtype.Generics

newtype ReactId = ReactId { unReactId :: (J.JSString, Int) }
    deriving (G.Generic, Read, Show, Eq, Ord)

instance Newtype ReactId

instance A.ToJSON ReactId where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON ReactId

instance A.ToJSONKey ReactId

instance A.FromJSONKey ReactId

-- reactIdJSRep :: ReactId -> JE.JSRep
-- reactIdJSRep = JE.toJSRep . J.pack . show

-----------------------------------------------

type ReactIdReader = MonadEnv ReactId
askReactId :: ReactIdReader m => m ReactId
askReactId = askEnv