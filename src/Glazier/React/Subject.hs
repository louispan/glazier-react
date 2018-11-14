module Glazier.React.Subject where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Glazier.React.Scene
import System.Mem.Weak

----------------------------------------------------------------------------------

-- | The IORef is for non-blocking reads.
-- The MVar is for synchronizing because it guarantees FIFO wakeup
-- which will help prevent old updates overriding new updates.
data Subject p = Subject
    { sceneRef :: IORef (Scene p)
    , sceneVar :: MVar (Scene p)
    } deriving Eq

data WeakSubject p = WeakSubject
    { sceneWeakRef :: Weak (IORef (Scene p))
    , sceneWeakVar :: Weak (MVar (Scene p))
    }

-- | Creates an IO action whose existence will keep the lease alive.
-- Running it has no side effects.
prolong :: Subject s -> IO ()
prolong (Subject scnRef scnVar) = do
    void $ readIORef scnRef
    void $ isEmptyMVar scnVar