{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.ReactId where

import Control.Monad.Context
import Control.Newtype.Generics
import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NE
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified JavaScript.Extras ()

newtype ReactId = ReactId { unReactId :: (NE.NonEmpty J.JSString, Int) }
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

type AskReactId = MonadAsk ReactId
askReactId :: AskReactId m => m ReactId
askReactId = askContext

type PutReactId = MonadPut ReactId
putReactId :: PutReactId m => ReactId -> m ()
putReactId = putContext

modifyReactId :: PutReactId m => (ReactId -> ReactId) -> m ()
modifyReactId = modifyContext

-- newtype ReactName = ReactName { unReactName :: J.JSString }
--     deriving (G.Generic, Read, Show, Eq, Ord)

-- instance Newtype ReactName

-- type ReactNameReader = MonadAsk ReactName
-- askReactName :: ReactNameReader m => m ReactName
-- askReactName = askContext
