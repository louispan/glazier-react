{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Trigger
    ( alwaysTick
    , tickOnce
    , alwaysOn
    , onceOn
    , alwaysOnRendered
    , onceOnRendered
    , trackRef
    ) where

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

postReadAction ::
    ( AsReactor cmd
    )
    => Lens' (Tagged "Once" (IO ()), Tagged "Always" (IO ())) (IO ())
    -> ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
postReadAction l go = do
    sbj <- view _subject
    Traversal slf <- view _self
    let go' = stamp' $ ReadScene sbj (magnifyModel slf go)
    post' $ MkAction go' $ \act ->
            let addListener = _scene._plan._doOnRendered.l %= (*> act)
            in stamp' $ TickScene sbj addListener

-- | Create a callback and add it to this gizmo's dlist of listeners.
postAction1 ::
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
postAction1 l sbj gid n goStrict goLazy = post' $
    MkAction1 goStrict goLazy $ \act ->
        -- save the created action handler in the gizmo
        let updateGizmo = _scene._plan._gizmos.at gid %= (Just . addListener . fromMaybe newGizmo)
            addListener = _listeners.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
            addAction acts = acts & l %~ (*> act)
        in stamp' $ TickScene sbj updateGizmo

postTickAction1 ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
postTickAction1 l gid n goStrict goLazy = do
    sbj <- view _subject
    delegate $ \fire ->
        let goLazy' a = stamp' @[]
                [ stamp' $ TickScene sbj (goLazy a >>= coerce fire)
                , stamp' $ Rerender sbj
                ]
        in postAction1 l sbj gid n goStrict goLazy'

postReadAction1 ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> ReaderT (Scene s) (State (DL.DList cmd)) ())
    -> Gadget cmd p s ()
postReadAction1 l gid n goStrict goLazy = do
    sbj <- view _subject
    Traversal slf <- view _self
    let goLazy' = stamp' . ReadScene sbj . magnifyModel slf . goLazy
    postAction1 l sbj gid n goStrict goLazy'

handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
handlesNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | Create stateful callback for a 'Notice' and add it to this gizmos's dlist of listeners.
-- The state function will be converted to a 'TickScene' followed by a 'Rerender' command.
alwaysTick ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
alwaysTick gid n goStrict =
    postTickAction1 (_2._Tagged' @"Always") gid n (handlesNotice goStrict)

-- | Variation of 'alwaysTick' that is called back only once.
tickOnce ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> (a -> State (Scenario cmd p) b)
    -> Gadget cmd p s b
tickOnce gid n goStrict =
    postTickAction1 (_1._Tagged @"Once") gid n (handlesNotice goStrict)

-- | Create a callback for a 'Notice' and add it to this gizmos's dlist of listeners.
-- Unlike 'alwaysTickOn', this does not result in 'TickScene' or 'Rerender'
-- but results in a 'ReadScene' command.
alwaysOn ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> (a -> ReaderT (Scene s) (State (DL.DList cmd)) ())
    -> Gadget cmd p s ()
alwaysOn gid n goStrict =
    postReadAction1 (_2._Tagged' @"Always") gid n (handlesNotice goStrict)

-- | Variation of 'alwaysOn' that is called back only once.
onceOn ::
    ( NFData a
    , AsReactor cmd
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> (a -> ReaderT (Scene s) (State (DL.DList cmd)) ())
    -> Gadget cmd p s ()
onceOn gid n goStrict =
    postReadAction1 (_1._Tagged' @"Once") gid n (handlesNotice goStrict)

-- | Register commands to call after every render.
-- Do not 'post'' 'TickScene' or 'Rerender' otherwise it will go into infinite render loops.
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
alwaysOnRendered = postReadAction (_2._Tagged' @"Always")

-- | Variation of 'alwaysOnRendered' that is called back only once.
onceOnRendered ::
    AsReactor cmd
    => ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> Gadget cmd p s ()
onceOnRendered = postReadAction (_1._Tagged' @"Once")

-- | This adds a ReactJS "ref" callback assign the ref into an 'EventTarget'
-- for the gizmo in the plan.
trackRef ::
    AsReactor cmd
    => GizmoId
    -> Gadget cmd p s ()
trackRef gid = postTickAction1 (_2._Tagged' @"Always") gid "ref" (pure . JE.fromJSR) hdlRef
  where
    hdlRef et = _scene._plan._gizmos.ix gid._targetRef .= et
