{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Plan.Internal where

import Control.Lens.Misc
import Control.Monad.Environ
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified GHC.Generics as G
import GHCJS.Foreign.Callback
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Component
import Glazier.React.ReactId
import JS.Data
import System.Mem.AnyStableName
import System.Mem.Weak

data RerenderRequired
    = RerenderNotRequired
    | RerenderRequired
    deriving (Show, Eq)

data WidgetCallbacks = WidgetCallbacks
    -- render function of the ReactComponent
    { widgetOnRender :: Callback (IO JSVal)
    -- updates the shimRef
    , widgetOnRef :: Listener
    } deriving (G.Generic)

makeLenses_ ''WidgetCallbacks

releaseWidgetCallbacks :: WidgetCallbacks -> IO ()
releaseWidgetCallbacks (WidgetCallbacks a b) = do
    releaseCallback a
    releaseCallback b

----------------------------------------------------------------------------------

-- | Interactivity data for a react component
data Plan = Plan
    { planId :: ReactId -- guaranteed unique
    , logName :: LogName
    , logLevel :: IO (Maybe LogLevel)
    , logDepth :: IO (Maybe (Maybe LogCallStackDepth))

    -- a javascript object to store/set miscellaneous data
    , scratch :: JSObject

    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    , widgetRef :: Maybe WidgetRef

    -- | cleanup to call DOM eventTarget.removeEventListener()
    , destructor :: IO ()

    -- The prerendered back buffer
    , prerendered :: JSVal

    -- An IO action that will update 'prerendered' with the latest markup using the associated 'Obj s'
    , prerender :: IO ()

    -- Optimization varaible: means whether the 'prerendered' frame is stale.
    , rerenderRequired :: RerenderRequired

    -- | notifiers that need to be unsubscribed from, when this is destroyed
    , notifiers :: M.Map ReactId (Weak (IORef Notifier))

    , handlers :: [(AnyStableName, Handler)]

    , listeners :: [(AnyStableName, Listener)]

    , widgetCallbacks :: WidgetCallbacks

    } deriving (G.Generic)

-- | Contains list of watchers that are interested in this model
data Notifier = Notifier
    { notifierId :: ReactId -- guaranteed unique
      -- watchers that need to be rerendered when this is mutated
    , watchers :: M.Map ReactId (Weak (IORef Plan))
    } deriving (G.Generic)

instance Show Notifier where
    showsPrec p pln = showParen
        (p >= 11)
        ( showString "Notifier "
            . showParen True (shows $ notifierId pln)
        )

makeLenses_ ''Notifier
makeLenses_ ''Plan

releasePlanCallbacks :: Plan -> IO ()
releasePlanCallbacks pln = do
    releaseWidgetCallbacks (widgetCallbacks pln)
    traverse_ (releaseCallback . snd) (listeners pln)

instance Show Plan where
    showsPrec p pln = showParen
        (p >= 11)
        ( showString "Plan "
            . showParen True (shows $ planId pln)
            . showParen True (shows $ logName pln)
        -- , showString ", " . showString "componentRef ? " . shows (isJust $ shimRef pln)
        )

type AskPlanWeakRef = MonadAsk' (Weak (IORef Plan))
askPlanWeakRef :: AskPlanWeakRef m => m (Weak (IORef Plan))
askPlanWeakRef = askEnv' @(Weak (IORef Plan))
-- localPlanWeakRef :: AskPlanWeakRef m => (Weak (IORef Plan) -> Weak (IORef Plan)) -> m a -> m a
-- localPlanWeakRef = localEnv' @(Weak (IORef Plan))

type AskNotifierWeakRef = MonadAsk' (Weak (IORef Notifier))
askNotifierWeakRef :: AskNotifierWeakRef m => m (Weak (IORef Notifier))
askNotifierWeakRef = askEnv' @(Weak (IORef Notifier))
-- localNotifierWeakRef :: AskNotifierWeakRef m => (Weak (IORef Notifier) -> Weak (IORef Notifier)) -> m a -> m a
-- localNotifierWeakRef = localEnv' @(Weak (IORef Notifier))

-- | Get the 'LogLevel' from a 'WeakRef' 'Plan'
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Maybe LogLevel) (Maybe LogLevel) (ReaderT (Weak (IORef Plan)) m) where
    askEnvP _ = do
        ref <- askPlanWeakRef
        liftIO $ go ref
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . logLevel $ pln

    -- -- | lie and don't actually use the given function to modify the environment
    -- -- Use 'localPlanWeakRef' instead
    -- localEnvP _ _ = id

-- | Get the 'LogName' from a 'WeakRef' 'Plan'
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk LogName LogName (ReaderT (Weak (IORef Plan)) m) where
    askEnvP _ = do
        ref <- askPlanWeakRef
        n <- liftIO $ go ref
        pure $ fromMaybe mempty n
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            pure $ logName pln

    -- -- | lie and don't actually use the given function to modify the environment
    -- -- Use 'localPlanWeakRef' instead
    -- localEnvP _ _ = id

-- | Get the 'LogCallStackDepth' from a 'WeakRef' 'Plan'
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Maybe (Maybe LogCallStackDepth)) (Maybe (Maybe LogCallStackDepth)) (ReaderT (Weak (IORef Plan)) m) where
    askEnvP _ = do
        ref <- askPlanWeakRef
        liftIO $ go ref
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . logDepth $ pln

    -- -- | lie and don't actually use the given function to modify the environment
    -- -- Use 'localPlanWeakRef' instead
    -- localEnvP _ _ = id
