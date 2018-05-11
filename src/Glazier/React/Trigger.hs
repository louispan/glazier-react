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
    , hdlElementalRef
    ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Defer
import Control.Monad.Reader
import Control.Monad.Trans.AState.Strict
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Tagged
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.MkId
import Glazier.React.Notice
import Glazier.React.Reactor
import Glazier.React.Scene
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- | Create a callback and add it to this elemental's dlist of listeners.
trigger_ ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> ElementalId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> Gadget cmd p s ())
    -> Gadget cmd p s ()
trigger_ l eid n goStrict goLazy = do
    ent@(Entity sbj _) <- ask
    let goLazy' = codify . unAStateT . (`evalGadgetT` ent) . goLazy
    postcmd' $ MkHandler1 goStrict goLazy' $ \act ->
        -- save the created action handler in the elemental
        let updateElemental = _plan._elementals.at eid %= (Just . addListener . fromMaybe newElemental)
            addListener = _listeners.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
            addAction acts = acts & l %~ (*> act)
        in command' $ TickScene sbj updateElemental

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- You probably want to use 'trigger'' since most React callbacks return a 'Notice',
-- except for the "ref" callback, in which case you probably want to use 'trackElemental'.
trigger ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> ElementalId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> Gadget cmd p s a
trigger l eid n goStrict = defer $ trigger_ l eid n goStrict

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
trigger' ::
    ( NFData a
    , AsReactor cmd
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> ElementalId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> Gadget cmd p s a
trigger' l eid n goStrict = trigger l eid n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | Register commands to call after a render.
-- Do not 'postcmd'' 'TickScene' or 'Rerender' otherwise it will go into infinite render loops.
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
    postcmd' $ MkHandler c $ \act ->
            -- save the rendered action handler in the plan
            let addListener = _plan._doOnRendered.l %= (*> act)
            in command' $ TickScene sbj addListener

-- | This adds a ReactJS "ref" callback assign the ref into an 'EventTarget'
-- for the elemental in the plan, so that the elemental '_targetRef' can be used.
hdlElementalRef ::
    AsReactor cmd
    => ElementalId
    -> Gadget cmd p s ()
hdlElementalRef eid = trigger _always eid "ref" (pure . JE.fromJSR) >>= hdlRef
  where
    hdlRef x = do
        sbj <- view _subject
        postcmd' . TickScene sbj $ _plan._elementals.ix eid._elementalRef .= x
