{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Trigger
    ( _once
    , _always
    , trigger
    , trigger'
    , onRendered
    , hdlGizmoRef
    ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Defer
import Control.Monad.Reader
import Control.Monad.Trans.AState.Strict
import Control.Monad.Trans.Maybe
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
trigger_ ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> Gadget cmd p s ())
    -> Gadget cmd p s ()
trigger_ l gid n goStrict goLazy = do
    ent@(Entity sbj _) <- ask
    let goLazy' = stamp' @[] . DL.toList . (`execAState` mempty) . (`evalGadgetT` ent) . goLazy
    post' $ MkHandler1 goStrict goLazy' $ \act ->
        -- save the created action handler in the gizmo
        let updateGizmo = _scene'._plan._gizmos.at gid %= (Just . addListener . fromMaybe newGizmo)
            addListener = _listeners.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
            addAction acts = acts & l %~ (*> act)
        in stamp' $ TickScenario sbj updateGizmo

-- | Create a callback for a 'JE.JSRep' and add it to this gizmos's dlist of listeners.
-- You probably want to use 'trigger'' since most React callbacks return a 'Notice',
-- except for the "ref" callback, in which case you probably want to use 'trackGizmo'.
trigger ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> Gadget cmd p s a
trigger l gid n goStrict = defer $ trigger_ l gid n goStrict

-- | Create a callback for a 'Notice' and add it to this gizmos's dlist of listeners.
trigger' ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> Gadget cmd p s a
trigger' l gid n goStrict = trigger l gid n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | Register commands to call after a render.
-- Do not 'post'' 'TickScenario' or 'Rerender' otherwise it will go into infinite render loops.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onRendered ::
    ( AsReactor cmd
    )
    => Lens' (Tagged "Once" (IO ()), Tagged "Always" (IO ())) (IO ())
    -> cmd
    -> Gadget cmd p s ()
onRendered l c = do
    sbj <- view _subject
    post' $ MkHandler c $ \act ->
            -- save the rendered action handler in the plan
            let addListener = _scene'._plan._doOnRendered.l %= (*> act)
            in stamp' $ TickScenario sbj addListener

-- | This adds a ReactJS "ref" callback assign the ref into an 'EventTarget'
-- for the gizmo in the plan, so that the gizmo '_targetRef' can be used.
hdlGizmoRef ::
    AsReactor cmd
    => GizmoId
    -> Gadget cmd p s ()
hdlGizmoRef gid = trigger _always gid "ref" (pure . JE.fromJSR) >>= hdlRef
  where
    hdlRef x = do
        sbj <- view _subject
        post' . TickScenario sbj $ _scene'._plan._gizmos.ix gid._gizmoRef .= x
