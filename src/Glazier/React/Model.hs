{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Model where

-- import Control.Lens
import Control.Applicative
import Control.Lens.Misc
import Control.Monad.Benign
import Control.Monad.Context
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Extras
import Data.Foldable
import Data.IORef
-- import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.StableNameMap
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Logger
import Glazier.React.EventTarget
import Glazier.React.ReactId
import Glazier.React.Shim
import System.Mem.Weak

----------------------------------------------------------------------------------

-- -- | React interactivity for a particular DOM element.
-- data Reactant = Reactant
--     -- elementalRef will be assigned the eventTarget of the react "ref" callback
--     { reactRef :: Maybe EventTarget
--     -- (name of event (eg. "ref", "onClick"), handler of event)
--     , reactListeners :: M.Map J.JSString
--         ( J.Callback (J.JSVal -> IO ())
--         , IORef (J.JSVal -> IO (), IO ())
--         )
--     } deriving (G.Generic)

-- makeLenses_ ''Reactant

----------------------------------------------------------------------------------

data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { shimOnRender :: J.Callback (IO J.JSVal)
    -- Run the mountedListener in the plan
    , shimOnMounted :: J.Callback (IO ())
    , shimOnUnmounted :: J.Callback (IO ())
    -- Run the renderedListener in the plan
    , shimOnRendered :: J.Callback (IO ())
    -- updates the shimRef
    , shimOnRef :: J.Callback (J.JSVal -> IO ())
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

data Rerendering
    = RerenderNotRequired
    | RerenderScheduled
    deriving (Show, Eq)

-- | Interactivity data for a react component
data Plan = Plan
    { planId :: ReactId
    , planLogLevel :: Benign IO (Maybe LogLevel)
    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    , shimRef :: Maybe ShimRef
    , shimCallbacks :: ShimCallbacks
    , createdCallbacks :: StableNameMap (J.JSVal -> IO ()) (J.Callback (J.JSVal -> IO ()))


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
    , renderedOnceListener :: IO ()

    -- interactivity data for child DOM elements
    -- , reactants :: M.Map ReactId Reactant
    -- , reactants :: M.Map ReactId Reactant

    -- FIXME: TODO
    -- interactivity for explicit eventTarget.addEventListener() callbacks
    -- , domlListeners :: M.Map ReactId
    --     ( J.Callback (J.JSVal -> IO ())
    --     , IORef (J.JSVal -> IO (), IO ())
    --     )

    -- cleanup to call DOM eventTarget.removeEventListener()
    -- , finalCleanup :: IO ()


    -- set of all reactIds that are modifying this widget
    , mutations :: S.Set EventTarget

    -- if rerender is required
    -- true if rerendering was scheduled
    -- false if rerendering was not scheduled
    -- prevents multiple scheduing of the rerender request
    , rerendering :: Rerendering


    -- FIXME: make rendering is two phase
    -- onRender simply uses  pre-rendered jsval ("deosn't block")
    -- but on mutated will refresh he pre-rendered jsval
    -- this way it can "block"

    -- The prerendered back buffer
    , prerendered :: J.JSVal
    -- An IO action will update 'prerendered' with the latest markup using the associated 'Obj s'
    , rerender :: IO ()

    } deriving (G.Generic)

makeLenses_ ''Plan

releasePlanCallbacks :: Plan -> IO ()
releasePlanCallbacks pln = do
    releaseShimCallbacks (shimCallbacks pln)
    traverse_ J.releaseCallback (createdCallbacks pln)

instance Show Plan where
    showsPrec d pln = showParen
        (d >= 11)
        ( showString "Plan {" . showString "componentRef ? " . shows (isJust $ shimRef pln)
        -- . showString ", " . showString "elementalIds = " . showList (M.keys $ reactants pln)
        -- . showString ", " . showString "planIds = " . showList (M.keys $ reactants pln)
        . showString "}"
        )

----------------------------------------------------------------------------------

-- -- | A 'Model' contains interactivity data for all widgets as well as the meta data.
-- data Model s = Model
--     { plan :: Plan
--     , meta :: s
--     } deriving (G.Generic, Show, Functor)

-- _meta :: Lens (Model s) (Model s') s s'
-- _meta = lens meta (\s a -> s { meta = a})

-- _plan :: Lens' (Model s) Plan
-- _plan = lens plan (\s a -> s { plan = a})

-- editModel :: (Functor f) => LensLike' f s a -> LensLike' f (Model s) (Model a)
-- editModel l safa s = (\s' -> s & _meta .~ s' ) <$> l afa' (s ^. _meta)
--   where
--     afa' a = (view _meta) <$> safa (s & _meta .~ a)

-- magnifiedModel ::
--     ( Magnify m n (Model a) (Model b)
--     , Functor (Magnified m r)
--     )
--     => LensLike' (Magnified m r) b a -> m r -> n r
-- magnifiedModel l = magnify (editModel l)

-- zoomedModel ::
--     ( Zoom m n (Model a) (Model b)
--     , Functor (Zoomed m r)
--     )
--     => LensLike' (Zoomed m r) b a -> m r -> n r
-- zoomedModel l = zoom (editModel l)

----------------------------------------------------------------------------------

-- | Avoids ambiguous types for 'askModel' and 'askModelWeakRef'
newtype Model s = Model { getModel :: s }

-- class MonadAsk s m => AskMeta s m where
--     askMeta :: m s
--     askMeta = askContext

-- reactTarget :: ReactId -> Traversal' (Model s) EventTarget
-- reactTarget k = _plan._reactants.ix k._reactRef._Just

-- type AskModel s = MonadAsk' Model s
-- askModel :: AskModel s m => m (Model s)
-- askModel = askContext'

-- type PlanRef s = IORef Plan
-- type WeakPlanRef s = Weak (IORef Plan)
-- type ModelRef s = IORef s
type Ref s = IORef s
type WeakRef s = Weak (IORef s)

-- benignReadWeakObjModel :: MonadBenignIO m => WeakObj s -> m (Maybe (Model s))
-- benignReadWeakObjModel obj = runMaybeT $ do
--     mdlRef <- MaybeT . liftBenignIO . benignDeRefWeak $ sceneWeakRef obj
--     liftBenignIO $ benignReadIORef mdlRef

-----------------------------------------------

type AskPlanWeakRef = MonadAsk (WeakRef Plan)
askPlanWeakRef :: AskPlanWeakRef m => m (WeakRef Plan)
askPlanWeakRef = askContext

-- type AskPlan = MonadAsk Plan
askPlan :: (MonadBenignIO m, Alternative m, AskPlanWeakRef m) => m Plan
askPlan = do
    wk <- askPlanWeakRef
    ref <- maybeM . liftBenignIO . Benign $ deRefWeak wk
    liftBenignIO . Benign $ readIORef ref

type AskModelWeakRef s = MonadAsk (Model (WeakRef s))
askModelWeakRef :: AskModelWeakRef s m => m (WeakRef s)
askModelWeakRef = getModel <$> askContext

-- type AskModel s = MonadAsk (Model s)
askModel :: (MonadBenignIO m, Alternative m, AskModelWeakRef s m) => m s
askModel = do
    wk <- askModelWeakRef
    ref <- maybeM . liftBenignIO . Benign $ deRefWeak wk
    liftBenignIO . Benign $ readIORef ref


-- | We can get the loglevel from a PlanWeakRef
instance Monad m => MonadAsk (Benign IO (Maybe LogLevel)) (ReaderT (WeakRef Plan) m) where
    -- logLevel :: m (Benign IO (Maybe LogLevel))
    askContext = do
        ref <- askPlanWeakRef
        pure $ go ref
      where
        go wk = Benign $ runMaybeT $ do
            ref <- MaybeT $ deRefWeak wk
            pln <- lift $ readIORef ref
            MaybeT . getBenign . planLogLevel $ pln
