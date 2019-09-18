{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Plan.Internal where

import Control.Lens.Misc
import Control.Monad.Context
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tagged.Extras
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.ReactId
import Glazier.React.Shim
import System.Mem.AnyStableName
import System.Mem.Weak

data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { shimOnRender :: J.Callback (IO J.JSVal)
    -- updates the shimRef
    , shimOnRef :: Listener
    -- Run the renderedListener in the plan
    , shimOnRendered :: J.Callback (IO ())

    } deriving (G.Generic)

makeLenses_ ''ShimCallbacks

releaseShimCallbacks :: ShimCallbacks -> IO ()
releaseShimCallbacks (ShimCallbacks a b c) = do
    J.releaseCallback a
    J.releaseCallback b
    J.releaseCallback c

----------------------------------------------------------------------------------

-- | Interactivity data for a react component
data Plan = Plan
    -- guaranteed unique for every widget instance
    { reactId :: ReactId
    , logName :: LogName
    , logLevel :: IO (Maybe LogLevel)
    , logDepth :: IO (Maybe (Maybe LogCallStackDepth))

    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    , shimRef :: Maybe ShimRef

    -- The prerendered back buffer
    , prerendered :: J.JSVal
    -- An IO action that will update 'prerendered' with the latest markup using the associated 'Obj s'
    , prerender :: IO ()

    -- Optimization varaible: means whether the 'prerendered' frame is stale.
    , rerenderRequired :: RerenderRequired

    -- | watchers that need to be rerendered when this is mutated
    , watchers :: M.Map ReactId (Weak (IORef Plan))

    -- | notifiers that need to be unsubscribed from when this is destroyed
    , notifiers :: M.Map ReactId (Weak (IORef Plan))

    -- Called after every rendered call fromReact
    , rendered :: IO ()

    -- | cleanup to call DOM eventTarget.removeEventListener()
    , destructor :: IO ()

    , handlers :: [(AnyStableName, Handler)]

    , listeners :: [(AnyStableName, Listener)]

    , shimCallbacks :: ShimCallbacks

    } deriving (G.Generic)

makeLenses_ ''Plan

releasePlanCallbacks :: Plan -> IO ()
releasePlanCallbacks pln = do
    releaseShimCallbacks (shimCallbacks pln)
    traverse_ (J.releaseCallback . snd) (listeners pln)

instance Show Plan where
    showsPrec p pln = showParen
        (p >= 11)
        ( showString "Plan "
            . showParen True (shows $ reactId pln)
            . showParen True (shows $ logName pln)
        -- , showString ", " . showString "componentRef ? " . shows (isJust $ shimRef pln)
        )

type AskPlanWeakRef = MonadAsk (Weak (IORef Plan))
askPlanWeakRef :: AskPlanWeakRef m => m (Weak (IORef Plan))
askPlanWeakRef = askContext

-- | Get the 'LogLevel' from a 'WeakRef' 'Plan'
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Maybe LogLevel) (ReaderT (Weak (IORef Plan)) m) where
    askContext = do
        ref <- askPlanWeakRef
        liftIO $ go ref
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . logLevel $ pln

-- | Get the 'LogName' from a 'WeakRef' 'Plan'
type AskLogName = MonadAsk LogName
askLogName :: AskLogName m => m LogName
askLogName = askContext
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Tagged "LogName" J.JSString) (ReaderT (Weak (IORef Plan)) m) where
    askContext = do
        ref <- askPlanWeakRef
        n <- liftIO $ go ref
        pure $ fromMaybe mempty n
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            pure $ logName pln

-- | Get the 'LogCallStackDepth' from a 'WeakRef' 'Plan'
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Maybe (Maybe LogCallStackDepth)) (ReaderT (Weak (IORef Plan)) m) where
    askContext = do
        ref <- askPlanWeakRef
        liftIO $ go ref
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . logDepth $ pln
