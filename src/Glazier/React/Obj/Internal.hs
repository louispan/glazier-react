{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.React.Obj.Internal where

import Control.Concurrent
import Control.Monad.Morph
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.IORef
import Glazier.Benign
import Glazier.Benign.Internal
import Glazier.React.Model
import System.Mem.Weak

data WeakObj s = WeakObj (Weak (IORef (Model s))) (Weak (MVar (Model s)))

-- | read-only accessor
modelWeakRef :: WeakObj s -> Weak (IORef (Model s))
modelWeakRef (WeakObj r _) = r

-- | read-only accessor
modelWeakVar :: WeakObj s -> Weak (MVar (Model s))
modelWeakVar (WeakObj _ v) = v

-- class GetWeakObj c s | c -> s where
--     _weakObj :: Getter c (WeakObj s)
--     _weakObj = to weakObj
--     weakObj :: c -> WeakObj s

-- instance GetWeakObj (WeakObj s) s where
--     weakObj = id

-- | Something with a ref for nonblocking reads
-- and a MVar for synchronized updates
-- as well as the corresponding 'Weak' pointer.
-- This is so that IO is not required to get the weak pointer.
data Obj s = Obj
    (WeakObj s)
    (IORef (Model s))
    (MVar (Model s))

instance Eq (Obj s) where
    (Obj _ _ x) == (Obj _ _ y) = x == y

-- instance GetWeakObj (Obj s) s where
--     weakObj (Obj s _ _) = s

-- | read-only accessor
modelRef :: Obj s -> IORef (Model s)
modelRef (Obj _ r _) = r

-- | read-only accessor
modelVar :: Obj s -> MVar (Model s)
modelVar (Obj _ _ v) = v

-- | read-only accessor
weakObj :: Obj s -> WeakObj s
weakObj (Obj w _ _) = w

deRefWeakObj :: MonadIO m => WeakObj s -> MaybeT m (Obj s)
deRefWeakObj obj = Obj obj <$> mdlRef <*> mdlVar
  where
    mdlWkRef = modelWeakRef obj
    mdlWkVar = modelWeakVar obj
    mdlRef = MaybeT . liftIO . deRefWeak $ mdlWkRef
    mdlVar = MaybeT . liftIO . deRefWeak $ mdlWkVar

benignDeRefWeakObj :: MonadIO m => WeakObj s -> MaybeT (Benign m) (Obj s)
benignDeRefWeakObj = hoist Benign . deRefWeakObj
