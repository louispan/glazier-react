module Glazier.React.Subject where

import Control.Concurrent
import Glazier.React.Scene
import Data.IORef
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
