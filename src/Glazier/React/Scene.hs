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

module Glazier.React.Scene where

import Control.Lens
import Control.Lens.Misc
import Control.Monad.Benign
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Logger
import Glazier.React.EventTarget
import Glazier.React.ReactId
import Glazier.React.Shim

----------------------------------------------------------------------------------

-- | React interactivity for a particular DOM element.
data Reactant = Reactant
    -- elementalRef will be assigned the eventTarge of the react "ref" callback
    { reactRef :: Maybe EventTarget
    -- (name of event (eg. "ref", "onClick"), handler of event)
    , reactListeners :: M.Map J.JSString
        ( J.Callback (J.JSVal -> IO ())
        , IORef (J.JSVal -> IO (), IO ())
        )
    } deriving (G.Generic)

makeLenses_ ''Reactant

----------------------------------------------------------------------------------

data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { shimOnRender :: J.Callback (IO J.JSVal)
    -- Run the mountedListener in the plan
    , shimOnMounted :: J.Callback (IO ())
    -- Run the renderedListener in the plan
    , shimOnRendered :: J.Callback (IO ())
    -- updates the shimRef
    , shimOnRef :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeLenses_ ''ShimCallbacks

releaseShimCallbacks :: ShimCallbacks -> IO ()
releaseShimCallbacks (ShimCallbacks a b c d) = do
    J.releaseCallback a
    J.releaseCallback b
    J.releaseCallback c
    J.releaseCallback d

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
    -- called after state was just updated
    , mutatedListener :: ReactId -> IO ()
    -- called after every rendering
    , renderedListener :: IO ()
    -- called after first rendering only
    , mountedListener :: IO ()
    -- do on next rerender. This gets reset after every rerender.
    , renderedOnceListener :: IO ()
    -- interactivity data for child DOM elements
    , reactants :: M.Map ReactId Reactant
    -- interactivity for explicit eventTarget.addEventListener() callbacks
    , domlListeners :: M.Map ReactId
        ( J.Callback (J.JSVal -> IO ())
        , IORef (J.JSVal -> IO (), IO ())
        )
    -- cleanup to call eventTarget.removeEventListener()
    , finalCleanup :: IO ()
    -- set of all reactIds that are modifying this widget
    , mutations :: S.Set ReactId
    -- if rerender is required
    -- true if rerendering was scheduled
    -- false if rerendering was not scheduled
    -- prevents multiple scheduing of the rerender request
    , rerendering :: Rerendering
    } deriving (G.Generic)

makeLenses_ ''Plan

releasePlanCallbacks :: Plan -> IO ()
releasePlanCallbacks pln = do
    releaseShimCallbacks (shimCallbacks pln)
    traverse_ (traverse (J.releaseCallback . fst) . reactListeners) (reactants pln)
    traverse_ (J.releaseCallback . fst) (domlListeners pln)

instance Show Plan where
    showsPrec d pln = showParen
        (d >= 11)
        ( showString "Plan {" . showString "componentRef ? " . shows (isJust $ shimRef pln)
        . showString ", " . showString "elementalIds = " . showList (M.keys $ reactants pln)
        . showString ", " . showString "planIds = " . showList (M.keys $ reactants pln)
        . showString "}"
        )

----------------------------------------------------------------------------------

-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
data Scene s = Scene
    { plan :: Plan
    , model :: s
    } deriving (G.Generic, Show, Functor)

_model :: Lens (Scene s) (Scene s') s s'
_model = lens model (\s a -> s { model = a})

_plan :: Lens' (Scene s) Plan
_plan = lens plan (\s a -> s { plan = a})

editScene :: (Functor f) => LensLike' f s a -> LensLike' f (Scene s) (Scene a)
editScene l safa s = (\s' -> s & _model .~ s' ) <$> l afa' (s ^. _model)
  where
    afa' a = (view _model) <$> safa (s & _model .~ a)

magnifiedScene ::
    ( Magnify m n (Scene a) (Scene b)
    , Functor (Magnified m r)
    )
    => LensLike' (Magnified m r) b a -> m r -> n r
magnifiedScene l = magnify (editScene l)

zoomedScene ::
    ( Zoom m n (Scene a) (Scene b)
    , Functor (Zoomed m r)
    )
    => LensLike' (Zoomed m r) b a -> m r -> n r
zoomedScene l = zoom (editScene l)

----------------------------------------------------------------------------------

elementTarget :: ReactId -> Traversal' (Scene s) EventTarget
elementTarget k = _plan._reactants.ix k._reactRef._Just
