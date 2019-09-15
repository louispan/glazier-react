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
import Data.Function.Extras
import Data.IORef
import Data.Maybe
import Data.Tagged.Extras
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.ReactId
import Glazier.React.Shim
import System.Mem.Weak

-- FIXME: field naming, add Tagged types?
data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { shimOnRender :: J.Callback (IO J.JSVal)
    -- updates the shimRef
    , shimOnRef :: J.Callback (J.JSVal -> IO ())
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
    , logName :: Tagged "LogName" J.JSString
    , logLevel :: IO (Maybe LogLevel)
    , logDepth :: IO (Maybe (Maybe LogCallStackDepth))

    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    , shimRef :: Maybe ShimRef

    -- The prerendered back buffer
    , prerendered :: J.JSVal
    -- An IO action that will update 'prerendered' with the latest markup using the associated 'Obj s'
    , prerender :: IO ()

    -- Whether to notify React of he updated 'prerenderd' frame
    , rerenderRequired :: RerenderRequired

    -- FIXME: TODO
    -- called after state was just updated
    -- The first arg is the html node that triggers the event
    -- , mutatedListener :: EventTarget -> IO ()

    -- set of all reactIds that are modifying this widget
    -- , mutations :: S.Set EventTarget

    -- if rerender is required
    -- true if rerendering was scheduled
    -- false if rerendering was not scheduled
    -- prevents multiple scheduing of the rerender request
    -- , rerendering :: Rerendering

    -- FIXME: make rendering is two phase
    -- onRender simply uses  pre-rendered jsval ("deosn't block")
    -- but on mutated will refresh he pre-rendered jsval
    -- this way it can "block"

    -- Called after every rendered call fromReact
    , rendered :: IO ()

    -- | cleanup to call DOM eventTarget.removeEventListener()
    , destructor :: IO ()

    , shimCallbacks :: ShimCallbacks
    -- , createdCallbacks :: StableNameMap (J.JSVal -> IO ()) (J.Callback (J.JSVal -> IO ()))
    -- , createdCallbacks :: [ (J.JSVal -> IO ()) (J.Callback (J.JSVal -> IO ()))]

    } deriving (G.Generic)

makeLenses_ ''Plan

releasePlanCallbacks :: Plan -> IO ()
releasePlanCallbacks pln = do
    hack $ releaseShimCallbacks (shimCallbacks pln)
    -- FIXME
    -- traverse_ J.releaseCallback (createdCallbacks pln)

instance Show Plan where
    showsPrec p pln = showParen
        (p >= 11)
        ( showString "Plan "
            . showParen True (shows $ reactId pln)
            . showParen True (shows $ logName pln)
        -- , showString ", " . showString "componentRef ? " . shows (isJust $ shimRef pln)
        )

type AskPlanWeakRef = MonadAsk (WeakRef Plan)
askPlanWeakRef :: AskPlanWeakRef m => m (WeakRef Plan)
askPlanWeakRef = askContext

-- | Get the 'LogLevel' from a 'WeakRef' 'Plan'
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Maybe LogLevel) (ReaderT (WeakRef Plan) m) where
    askContext = do
        ref <- askPlanWeakRef
        liftIO $ go ref
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . logLevel $ pln

-- | Get the 'LogName' from a 'WeakRef' 'Plan'
type AskLogName = MonadAsk (Tagged "LogName" J.JSString)
askLogName :: AskLogName m => m (Tagged "LogName" J.JSString)
askLogName = askContext
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Tagged "LogName" J.JSString) (ReaderT (WeakRef Plan) m) where
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
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Maybe (Maybe LogCallStackDepth)) (ReaderT (WeakRef Plan) m) where
    askContext = do
        ref <- askPlanWeakRef
        liftIO $ go ref
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . logDepth $ pln
