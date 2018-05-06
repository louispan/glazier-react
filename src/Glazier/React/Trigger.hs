{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.React.Trigger
    ( trigger
    , triggerOnce
    , trigger1
    , triggerOnce1
    , triggerOnUpdated
    , triggerOnceOnUpdated
    , withRef
    ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.AState.Strict
import Data.Coerce
import qualified Data.DList as DL
import Data.Maybe
import Data.Tagged
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Gadget
import Glazier.React.MkId
import Glazier.React.Notice
import Glazier.React.Reactor
import Glazier.React.Scene
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- | Create a callback and add it to this gizmo's dlist of listeners.
-- NB. You probably want to use 'trigger' instead since most React callbacks
-- generate a 'Notice'.
-- Only the "ref" callback generate 'EventTarget' or 'ComponentRef' in which case you would want
-- to use 'withRef' instead.
mkTriggerAction1 ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Every" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
mkTriggerAction1 l gid n goStrict goLazy = do
    sbj <- view _subject
    delegate $ \fire -> do
        -- Add extra command producting state actions at the end
        let goLazy' a = stamp' $ TickState sbj (goLazy a >>= coerce fire)
        post' $ MkAction1 goStrict goLazy' $ \act ->
                let updateGizmo = _scene._plan._gizmos.at gid %= (Just . addListener . fromMaybe newGizmo)
                    addListener = _listeners.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
                    addAction acts = acts & l %~ (*> act)
                in stamp' $ TickState sbj updateGizmo

mkUpdatedAction ::
    AsReactor cmd
    => Lens' (Tagged "Once" (IO ()), Tagged "Every" (IO ())) (IO ())
    -> ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
mkUpdatedAction l go = do
    sbj <- view _subject
    Traversal slf <- view _self
    let go' = stamp' $ ReadState sbj (magnifyModel slf go)
    post' $ MkAction go' $ \act ->
            let addListener = _scene._plan._doOnUpdated.l %= (*> act)
            in stamp' $ TickState sbj addListener

triggerOnUpdated_ ::
    AsReactor cmd
    => Lens' (Tagged "Once" (IO ()), Tagged "Every" (IO ())) (IO ())
    -> ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
triggerOnUpdated_ l go = mkUpdatedAction l go

-- | Register commands to call after every render.
-- Do not post 'TickState' otherwise you will go into infinite render loops.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
triggerOnUpdated ::
    AsReactor cmd
    => ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
triggerOnUpdated = triggerOnUpdated_ (_2._Wrapped' @(Tagged "Every" _))

triggerOnceOnUpdated ::
    AsReactor cmd
    => ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
triggerOnceOnUpdated = triggerOnUpdated_ (_1._Wrapped' @(Tagged "Once" _))

-- | A 'trigger1' where all event info is dropped and the given value is fired.
trigger ::
    AsReactor cmd
    => GizmoId
    -> J.JSString
    -> State (Scenario cmd p) b
    -> Gadget cmd p s b
trigger = trigger_ (_2._Wrapped' @(Tagged "Every" _))

triggerOnce ::
    AsReactor cmd
    => GizmoId
    -> J.JSString
    -> State (Scenario cmd p) b
    -> Gadget cmd p s b
triggerOnce = trigger_ (_1._Wrapped' @(Tagged "Once" _))

-- | Create callback for 'Notice' and add it to this gizmos's dlist of listeners.
trigger1 ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
trigger1 = trigger1_ (_2._Wrapped' @(Tagged "Every" _))

triggerOnce1 ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
triggerOnce1 = trigger1_ (_1._Wrapped' @(Tagged "Once" _))

trigger1_ ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Every" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
trigger1_ l gid n goStrict goLazy = mkTriggerAction1 l gid n goStrict' goLazy
  where
    goStrict' e = case JE.fromJSR e of
        Nothing -> pure Nothing
        Just e' -> Just <$> goStrict e'

trigger_ ::
    AsReactor cmd
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Every" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> State (Scenario cmd p) b
    -> Gadget cmd p s b
trigger_ l gid n go = mkTriggerAction1 l gid n (const $ pure (Just ())) (const $ go)

-- | This adds a ReactJS "ref" callback assign the ref into an EventTarget for the
-- gizmo in the plan
withRef ::
    AsReactor cmd
    => GizmoId
    -> Gadget cmd p s ()
withRef gid = mkTriggerAction1 (_2._Wrapped' @(Tagged "Every" _)) gid "ref" (pure . Just) hdlRef
  where
    hdlRef j = let evt = JE.fromJSR j
            in _scene._plan._gizmos.ix gid._targetRef .= evt
