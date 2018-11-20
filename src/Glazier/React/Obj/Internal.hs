module Glazier.React.ModelPtr.Internal where

import Control.Concurrent
import Data.IORef
import Glazier.React.Model
import System.Mem.Weak

-- | Something with a ref for nonblocking reads
-- and a MVar for synchronized updates
-- as well as the corresponding 'Weak' pointer.
-- This is so that IO is not required to get the weak pointer.
data ModelPtr a = ModelPtr
    (Weak (IORef (Model a)))
    (Weak (MVar (Model a)))
    (IORef (Model a))
    (MVar (Model a))

instance Eq (ModelPtr a) where
    (ModelPtr _ _ _ x) == (ModelPtr _ _ _ y) = x == y

modelWeakRef :: ModelPtr a -> Weak (IORef (Model a))
modelWeakRef (ModelPtr wr _ _ _) = wr

weakModelVar :: ModelPtr a -> Weak (MVar (Model a))
weakModelVar (ModelPtr _ wv _ _) = wv

modelRef :: ModelPtr a -> IORef (Model a)
modelRef (ModelPtr _ _ r _) = r

modelVar :: ModelPtr a -> MVar (Model a)
modelVar (ModelPtr _ _ _ v) = v

