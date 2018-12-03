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

import Control.Lens
import Control.Lens.Misc
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React.Component
import Glazier.React.EventTarget
import Glazier.React.ReactId

----------------------------------------------------------------------------------

-- | Interactivity for a particular DOM element.
data Elemental = Elemental
    { elementalRef :: Maybe EventTarget
    -- (name of event, context of event)
    , reactListeners :: M.Map J.JSString
        ( J.Callback (J.JSVal -> IO ())
        , IORef (J.JSVal -> IO (), IO ())
        )
    } deriving (G.Generic)

makeLenses_ ''Elemental

----------------------------------------------------------------------------------

data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { shimRender :: J.Callback (IO J.JSVal)
    -- Run the mountedListener in the plan
    , shimMounted :: J.Callback (IO ())
    -- Run the renderedListener in the plan
    , shimRendered :: J.Callback (IO ())
    -- updates the componenRef
    , shimRef :: J.Callback (J.JSVal -> IO ())
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
    -- | Rerendering not scheduled
    = RerenderNotRequired
    -- | Rerendering will be scheduled eventually.
    -- erender is suppressed, but something will be guaranteed
    -- to trigger another rerender, so it is safe to drop the current
    -- rerender request
    | RerenderSuppressed
    | RerenderRequired
    deriving (Show, Eq)

-- | Interactivity data for a react component
data Plan = Plan
    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    { planId :: ReactId
    , componentRef :: Maybe ComponentRef
    , shimCallbacks :: ShimCallbacks
    -- called after state was just updated
    , mutatedListener :: ReactId -> IO ()
    -- called after every rendering
    , renderedListener :: IO ()
    -- called after first rendering only
    , mountedListener :: IO ()
    -- do on next rerender. This gets reset after every rerender.
    , nextRenderedListener :: IO ()
    -- interactivity data for child DOM elements
    , elementals :: M.Map ReactId Elemental
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
    , rerendering :: Rerendering
    } deriving (G.Generic)

makeLenses_ ''Plan

instance Show Plan where
    showsPrec d pln = showParen
        (d >= 11)
        ( showString "Plan {" . showString "componentRef ? " . shows (isJust $ componentRef pln)
        . showString ", " . showString "elementalIds = " . showList (M.keys $ elementals pln)
        . showString ", " . showString "planIds = " . showList (M.keys $ elementals pln)
        . showString "}"
        )

----------------------------------------------------------------------------------

-- | A 'Model' contains interactivity data for all widgets as well as the model data.
data Model s = Model
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { plan :: Plan
    , model :: s
    } deriving (G.Generic, Show, Functor)

_model :: Lens (Model s) (Model s') s s'
_model = lens model (\s a -> s { model = a})

_plan :: Lens' (Model s) Plan
_plan = lens plan (\s a -> s { plan = a})

editModel :: (Functor f) => LensLike' f s a -> LensLike' f (Model s) (Model a)
editModel l safa s = (\s' -> s & _model .~ s' ) <$> l afa' (s ^. _model)
  where
    afa' a = (view _model) <$> safa (s & _model .~ a)

magnifiedModel ::
    ( Magnify m n (Model a) (Model b)
    , Functor (Magnified m r)
    )
    => LensLike' (Magnified m r) b a -> m r -> n r
magnifiedModel l = magnify (editModel l)

zoomedModel ::
    ( Zoom m n (Model a) (Model b)
    , Functor (Zoomed m r)
    )
    => LensLike' (Zoomed m r) b a -> m r -> n r
zoomedModel l = zoom (editModel l)

----------------------------------------------------------------------------------

elementTarget :: ReactId -> Traversal' (Model s) EventTarget
elementTarget k = _plan._elementals.ix k._elementalRef._Just
