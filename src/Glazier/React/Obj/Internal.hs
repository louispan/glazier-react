{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Obj.Internal where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Trans.Extras
import Data.IORef
import qualified GHC.Generics as G
import Glazier.React.Plan.Internal
import System.Mem.Weak

----------------------------------------------------------------------------------

data Obj s = Obj
    -- Plan is one per widget
    (IORef Plan) (Weak (IORef Plan))
    -- Notifier and @s@ is one per model
    (IORef Notifier) (Weak (IORef Notifier))
    (MVar s) (Weak (MVar s))
    deriving (G.Generic)

data WeakObj s = WeakObj
    (Weak (IORef Plan))
    (Weak (IORef Notifier))
    (Weak (MVar s))
    deriving (G.Generic)

weakObj :: Obj s -> WeakObj s
weakObj (Obj _ plnWk _ nfrWk _ mdlWk) = WeakObj plnWk nfrWk mdlWk

_weakObj :: (Profunctor p, Contravariant f) => Optic' p f (Obj s) (WeakObj s)
_weakObj = to weakObj

fromWeakObj :: AlternativeIO m => WeakObj s -> m (Obj s)
fromWeakObj (WeakObj plnWkRef notifierWkRef mdlWkVar) = do
    plnRef <- guardJustIO $ deRefWeak plnWkRef
    notifierRef <- guardJustIO $ deRefWeak notifierWkRef
    mdlVar <- guardJustIO $ deRefWeak mdlWkVar
    pure $ Obj plnRef plnWkRef notifierRef notifierWkRef mdlVar mdlWkVar


