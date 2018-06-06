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

import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tagged
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React.Component
import Glazier.React.EventTarget
import Glazier.React.ReactId
import qualified JavaScript.Extras as JE

----------------------------------------------------------------------------------

type Always = Tagged "Always"
type Once = Tagged "Once"

_once :: Lens' (Once a, Always a) a
_once = (_1._Tagged' @"Once")

_always :: Lens' (Once a, Always a) a
_always = (_2._Tagged' @"Always")

-- | Interactivity for a particular DOM element.
data Elemental = Elemental
    { elementalRef :: Maybe EventTarget
    -- (name of event, context of event)
    , listeners :: M.Map J.JSString (Once (JE.JSRep -> IO ()), Always (JE.JSRep -> IO ()))
    } deriving (G.Generic)

makeLenses_ ''Elemental

newElemental :: Elemental
newElemental = Elemental Nothing mempty

----------------------------------------------------------------------------------

data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { shimRender :: J.Callback (IO J.JSVal)
    -- Run the doOnRendered in the plan
    , shimRendered :: J.Callback (IO ())
    -- updates the componenRef
    , shimRef :: J.Callback (J.JSVal -> IO ())
    -- all listeners use the same entry function, just a different
    -- first arg context.
    , shimListen :: J.Callback (J.JSVal -> J.JSVal -> IO ())
    } deriving (G.Generic)

makeLenses_ ''ShimCallbacks

instance CD.Dispose ShimCallbacks where
    dispose (ShimCallbacks a b c d) = CD.dispose a <> CD.dispose b <> CD.dispose c <> CD.dispose d

----------------------------------------------------------------------------------

-- | Interactivity data for a react component
data Plan = Plan
    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    { componentRef :: Maybe ComponentRef
    , shimCallbacks :: ShimCallbacks
    , doOnRendered :: (Once (IO ()), Always (IO ()))
    -- interactivity data for child DOM elements
    , elementals :: M.Map ReactId Elemental
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

-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
data Scene s = Scene
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { plan :: Plan
    , model :: s
    } deriving (G.Generic, Show, Functor)

_model :: Lens (Scene s) (Scene s') s s'
_model = lens model (\s a -> s { model = a})

_plan :: Lens' (Scene s) Plan
_plan = lens plan (\s a -> s { plan = a})

editSceneModel :: (Functor f) => LensLike' f s a -> LensLike' f (Scene s) (Scene a)
editSceneModel l safa s = (\s' -> s & _model .~ s' ) <$> l afa' (s ^. _model)
  where
    afa' a = (view _model) <$> safa (s & _model .~ a)

magnifiedScene ::
    ( Magnify m n (Scene a) (Scene b)
    , Functor (Magnified m r)
    )
    => LensLike' (Magnified m r) b a -> m r -> n r
magnifiedScene l = magnify (editSceneModel l)

zoomedScene ::
    ( Zoom m n (Scene a) (Scene b)
    , Functor (Zoomed m r)
    )
    => LensLike' (Zoomed m r) b a -> m r -> n r
zoomedScene l = zoom (editSceneModel l)

----------------------------------------------------------------------------------

elementTarget :: ReactId -> Traversal' (Scene s) EventTarget
elementTarget ri = _plan._elementals.ix ri._elementalRef._Just
