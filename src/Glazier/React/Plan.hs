{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Plan where

import Control.Lens.Misc
import Control.Monad.Context
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.IORef
import Data.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Logger
import Glazier.React.Shim
import Glazier.React.Type
import System.Mem.Weak

-- FIXME: field naming, add Tagged types?
data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { shimOnRender :: J.Callback (IO J.JSVal)
    -- updates the shimRef
    , shimOnRef :: J.Callback (J.JSVal -> IO ())
    -- Run the mountedListener in the plan
    , shimOnMounted :: J.Callback (IO ())
    , shimOnUnmounted :: J.Callback (IO ())
    -- Run the renderedListener in the plan
    , shimOnRendered :: J.Callback (IO ())

    } deriving (G.Generic)

makeLenses_ ''ShimCallbacks

releaseShimCallbacks :: ShimCallbacks -> IO ()
releaseShimCallbacks (ShimCallbacks a b c d e) = do
    J.releaseCallback a
    J.releaseCallback b
    J.releaseCallback c
    J.releaseCallback d
    J.releaseCallback e

----------------------------------------------------------------------------------

data Rerender
    = RerenderNotRequired
    | RerenderRequired
    deriving (Show, Eq)

-- | Interactivity data for a react component
data Plan = Plan
    { logName :: LogName J.JSString
    , logLevel :: IO (Maybe LogLevel)
    , logDepth :: IO (Maybe (Maybe LogCallStackDepth))

    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    , shimRef :: Maybe ShimRef

    -- The prerendered back buffer
    , prerendered :: J.JSVal
    -- An IO action that will update 'prerendered' with the latest markup using the associated 'Obj s'
    , prerender :: IO ()

    -- called after every rendering
    -- FIXME: TODO
    -- , mountedListeners :: (EventTarget -> IO ())
    -- , unmountedListeners :: (EventTarget -> IO ())
    -- , renderedListener :: (EventTarget -> IO ())

    -- FIXME: TODO
    -- called after state was just updated
    -- The first arg is the html node that triggers the event
    -- , mutatedListener :: EventTarget -> IO ()


    -- called after first rendering only

    -- do on next rerender. This gets reset after every rerender.
    -- used just to support 'Glazier.React.Reactor.getReactRef'
    -- , renderedOnceListener :: IO ()

    -- interactivity data for child DOM elements
    -- , reactants :: M.Map ReactId Reactant
    -- , reactants :: M.Map ReactId Reactant

    -- FIXME: TODO
    -- interactivity for explicit eventTarget.addEventListener() callbacks
    -- , domlListeners :: M.Map ReactId
    --     ( J.Callback (J.JSVal -> IO ())
    --     , IORef (J.JSVal -> IO (), IO ())
    --     )



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


    -- | cleanup to call DOM eventTarget.removeEventListener()
    -- 'destructor' is called before releasing callbacks
    , destructor :: IO ()

    , shimCallbacks :: ShimCallbacks
    -- , createdCallbacks :: StableNameMap (J.JSVal -> IO ()) (J.Callback (J.JSVal -> IO ()))
    -- , createdCallbacks :: [ (J.JSVal -> IO ()) (J.Callback (J.JSVal -> IO ()))]

    } deriving (G.Generic)

makeLenses_ ''Plan

releasePlanCallbacks :: Plan -> IO ()
releasePlanCallbacks pln = do
    releaseShimCallbacks (shimCallbacks pln)
    -- FIXME
    -- traverse_ J.releaseCallback (createdCallbacks pln)

instance Show Plan where
    showsPrec p pln = showParen
        (p >= 11)
        ( showString "Plan " . shows (logName pln)
        -- , showString ", " . showString "componentRef ? " . shows (isJust $ shimRef pln)
        -- . showString ", " . showString "elementalIds = " . showList (M.keys $ reactants pln)
        -- . showString ", " . showString "planIds = " . showList (M.keys $ reactants pln)
        -- . showString "}"
        )

type AskPlanWeakRef = MonadAsk (WeakRef Plan)
askPlanWeakRef :: AskPlanWeakRef m => m (WeakRef Plan)
askPlanWeakRef = askContext

-- | We can get the loglevel from a PlanWeakRef
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Maybe LogLevel) (ReaderT (WeakRef Plan) m) where
    askContext = do
        ref <- askPlanWeakRef
        liftIO $ go ref
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . logLevel $ pln

-- | We can get the LogName from a 'WeakRef' 'Plan'
type AskLogNameJS = AskLogName J.JSString
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (LogName J.JSString) (ReaderT (WeakRef Plan) m) where
    askContext = do
        ref <- askPlanWeakRef
        n <- liftIO $ go ref
        pure $ fromMaybe mempty n
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            pure $ logName $ pln

-- | We can get the 'LogCallStackDepth' from a 'WeakRef' 'Plan'
instance {-# OVERLAPPING #-} MonadIO m => MonadAsk (Maybe (Maybe LogCallStackDepth)) (ReaderT (WeakRef Plan) m) where
    askContext = do
        ref <- askPlanWeakRef
        liftIO $ go ref
      where
        go wk = runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . logDepth $ pln
