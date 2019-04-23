{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.ReactId where

import Control.Monad.Reader
import Control.Monad.Morph
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

class Monad m => ReactIdReader m where
    askReactId :: m ReactId
    localReactId :: (ReactId -> ReactId) -> m a -> m a

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MFunctor t, ReactIdReader m) => ReactIdReader (t m) where
    askReactId = lift askReactId
    localReactId f m = hoist (localReactId f) m

instance {-# OVERLAPPABLE #-} Monad m => ReactIdReader (ReaderT ReactId m) where
    askReactId = ask
    localReactId = local
