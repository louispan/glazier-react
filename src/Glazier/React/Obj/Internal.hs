{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.React.Obj.Internal where

import Control.Concurrent
import Control.Monad.Benign
import Control.Monad.Trans.Maybe
import Data.IORef
import Glazier.React.Scene
import System.Mem.Weak

data WeakObj s = WeakObj (Weak (IORef (Scene s))) (Weak (MVar (Scene s)))

-- | read-only accessor
sceneWeakRef :: WeakObj s -> Weak (IORef (Scene s))
sceneWeakRef (WeakObj r _) = r

-- | read-only accessor
sceneWeakVar :: WeakObj s -> Weak (MVar (Scene s))
sceneWeakVar (WeakObj _ v) = v

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
    (IORef (Scene s))
    (MVar (Scene s))

instance Eq (Obj s) where
    (Obj _ _ x) == (Obj _ _ y) = x == y

-- instance GetWeakObj (Obj s) s where
--     weakObj (Obj s _ _) = s

-- | read-only accessor
sceneRef :: Obj s -> IORef (Scene s)
sceneRef (Obj _ r _) = r

-- | read-only accessor
sceneVar :: Obj s -> MVar (Scene s)
sceneVar (Obj _ _ v) = v

-- | read-only accessor
weakObj :: Obj s -> WeakObj s
weakObj (Obj w _ _) = w

deRefWeakObj :: MonadBenignIO m => WeakObj s -> MaybeT m (Obj s)
deRefWeakObj obj = Obj obj <$> scnRef <*> scnVar
  where
    mdlWkRef = sceneWeakRef obj
    mdlWkVar = sceneWeakVar obj
    scnRef = MaybeT . liftBenignIO . benignDeRefWeak $ mdlWkRef
    scnVar = MaybeT . liftBenignIO . benignDeRefWeak $ mdlWkVar

-- benignDeRefWeakObj :: MonadIO m => WeakObj s -> Benign m (Maybe (Obj s))
-- benignDeRefWeakObj = Benign . deRefWeakObj
