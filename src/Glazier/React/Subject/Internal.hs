module Glazier.React.Subject.Internal where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Glazier.React.Scene
----------------------------------------------------------------------------------

-- | Using MVar for synchronizing because it guarantees FIFO wakeup
-- which will help prevent old updates overriding new updates.
-- The constructor is hidden to maintain the expectation that
-- the last two member will keep the 'ShimCallbacks' inside the 'Scene' alive.
-- Before removing Subject from a container, use 'prolong' to store
-- the prolonging IO action "nextRenderedListener" action handler,
-- so that the 'ShimCallbacks' is garbage collected
-- only after the child widget is no longer rendered.
data Subject p = Subject
    (IORef (Scene p))
    (MVar (Scene p))
    (IORef (MVar ())) -- garbage collection for rendering callback
    (MVar ()) -- garbage collection for other callbacks

    deriving Eq

sceneRef :: Subject p -> IORef (Scene p)
sceneRef (Subject x _ _ _) = x

sceneVar :: Subject p -> MVar (Scene p)
sceneVar (Subject _ x _ _) = x

renderLeaseRef :: Subject p -> IORef (MVar ())
renderLeaseRef (Subject _ _ x _) = x

otherLeaseVar :: Subject p -> MVar ()
otherLeaseVar (Subject _ _ _ x) = x

-- | Creates an IO action whose existence will keep the lease alive.
-- Running it has no side effects.
prolong :: Subject s -> IO ()
prolong (Subject _ _ rndrLeaseRef otherCbLease) = do
    rndrLease <- readIORef rndrLeaseRef
    (void $ isEmptyMVar rndrLease)
    (void $ isEmptyMVar otherCbLease)
