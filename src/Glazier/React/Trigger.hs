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

-- alwaysReviseOn
-- reviseOnceOn
-- alwaysOn
-- studyOn

-- alwaysOnRendered
-- onceOnRendered


module Glazier.React.Trigger where
    -- ( trigger
    -- , triggerOnce
    -- , trigger1
    -- , triggerOnce1
    -- , triggerOnUpdated
    -- , triggerOnceOnUpdated
    -- , withRef
    -- ) where

import Control.DeepSeq
import Control.Lens
import Control.Lens.Misc.Tagged
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.AState.Strict
import Control.Monad.Trans.Maybe
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
import Glazier.React.Subject
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- | Create a callback and add it to this gizmo's dlist of listeners.
-- NB. You probably want to use 'trigger' instead since most React callbacks
-- generate a 'Notice'.
-- The "React ref" callback generate 'EventTarget' or 'ComponentRef' in which case you would want
-- to use 'withRef' instead.
mkAction1_ ::
    ( NFData a
    , AsReactor cmd
    , MonadState s m
    , HasCommands s cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> Subject p
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> m ()
mkAction1_ l sbj gid n goStrict goLazy = post' $
    MkAction1 goStrict goLazy $ \act ->
        -- save the created action handler in the gizmo
        let updateGizmo = _scene._plan._gizmos.at gid %= (Just . addListener . fromMaybe newGizmo)
            addListener = _listeners.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
            addAction acts = acts & l %~ (*> act)
        in stamp' $ Revise sbj updateGizmo

addTriggerHandler ::
    ( AsReactor cmd
    )
    => Lens' (Tagged "Once" (IO ()), Tagged "Always" (IO ())) (IO ())
    -> ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
addTriggerHandler l go = do
    sbj <- view _subject
    Traversal slf <- view _self
    let go' = stamp' $ Study sbj (magnifyModel slf go)
    post' $ MkAction go' $ \act ->
            let addListener = _scene._plan._doOnRendered.l %= (*> act)
            in stamp' $ Revise sbj addListener

addTickHandler1 ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
addTickHandler1 l gid n goStrict goLazy = do
    sbj <- view _subject
    delegate $ \fire ->
        let goLazy' a = stamp' @[]
                [ stamp' $ Revise sbj (goLazy a >>= coerce fire)
                , stamp' $ Rerender sbj
                ]
        in mkAction1_ l sbj gid n goStrict goLazy'

addTriggerHandler1 ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> ReaderT (Scene s) (State (DL.DList cmd)) ())
    -> Gadget cmd p s ()
addTriggerHandler1 l gid n goStrict goLazy = do
    sbj <- view _subject
    Traversal slf <- view _self
    let goLazy' = stamp' . Study sbj . magnifyModel slf . goLazy
    mkAction1_ l sbj gid n goStrict goLazy'

handleNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
handleNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | Create stateful callback for a 'Notice' and add it to this gizmos's dlist of listeners.
-- The state function will be converted to a 'Revise' followed by a 'Rerender' command.
alwaysReviseOn ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
alwaysReviseOn gid n goStrict =
    addTickHandler1 (_2._Tagged' @"Always") gid n (handleNotice goStrict)

-- | Variation of 'alwaysReviseOn' that is called back only once.
reviseOnceOn ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
reviseOnceOn gid n goStrict =
    addTickHandler1 (_1._Tagged @"Once") gid n (handleNotice goStrict)

-- | Create a callback for a 'Notice' and add it to this gizmos's dlist of listeners.
-- Unlike 'alwaysTickOn', this does not result in 'Revise' or 'Rerender'
-- but results in a 'Study' command.
alwaysStudyOn ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> (a -> ReaderT (Scene s) (State (DL.DList cmd)) ())
    -> Gadget cmd p s ()
alwaysStudyOn gid n goStrict =
    addTriggerHandler1 (_2._Tagged' @"Always") gid n (handleNotice goStrict)

-- | Variation of 'alwaysStudyOn' that is called back only once.
studyOnceOn ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> (a -> ReaderT (Scene s) (State (DL.DList cmd)) ())
    -> Gadget cmd p s ()
studyOnceOn gid n goStrict =
    addTriggerHandler1 (_1._Tagged' @"Once") gid n (handleNotice goStrict)

-- | Register commands to call after every render.
-- Do not 'post'' 'Revise' or 'Rerender' otherwise it will go into infinite render loops.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
alwaysOnRendered ::
    AsReactor cmd
    => ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
alwaysOnRendered = addTriggerHandler (_2._Tagged' @"Always")

-- | Variation of 'alwaysOnRendered' that is called back only once.
onceOnRendered ::
    AsReactor cmd
    => ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
onceOnRendered = addTriggerHandler (_1._Tagged' @"Once")

-- | This adds a ReactJS "ref" callback assign the ref into an 'EventTarget'
-- for the gizmo in the plan.
trackRef ::
    AsReactor cmd
    => GizmoId
    -> Gadget cmd p s ()
trackRef gid = addTickHandler1 (_2._Tagged' @"Always") gid "ref" (pure . JE.fromJSR) hdlRef
  where
    hdlRef et = _scene._plan._gizmos.ix gid._targetRef .= et
