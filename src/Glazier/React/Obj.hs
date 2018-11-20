module Glazier.React.Obj where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Glazier.React.Model
import System.Mem.Weak

----------------------------------------------------------------------------------

-- | The IORef is for non-blocking reads.
-- The MVar is for synchronizing because it guarantees FIFO wakeup
-- which will help prevent old updates overriding new updates.
data Obj p = Obj
    { modelRef :: IORef (Model p)
    , modelVar :: MVar (Model p)
    } deriving Eq

data WeakObj p = WeakObj
    { modelWeakRef :: Weak (IORef (Model p))
    , modelWeakVar :: Weak (MVar (Model p))
    }

-- | Creates an IO action whose existence will keep the lease alive.
-- Running it has no side effects.
prolong :: Obj s -> IO ()
prolong (Obj scnRef scnVar) = do
    void $ readIORef scnRef
    void $ isEmptyMVar scnVar