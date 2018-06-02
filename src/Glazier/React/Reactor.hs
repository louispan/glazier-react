{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Reactor
    ( AsReactor
    , MonadReactor
    , ReactorCmd -- constructor not exported
    , SceneState
    , mkReactId
    , mkSubject
    , mkSubject'
    -- , addSubject
    , cleanupSubject
    , rerender
    , getScene
    , tickScene
    , tickSceneThen
    , trigger
    , trigger'
    , trigger_
    , onRendered
    , hdlElementalRef
    ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import Data.Maybe
import Data.Tagged
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.Notice
import Glazier.React.ReactId
import Glazier.React.Reactor.Internal
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Widget
import qualified JavaScript.Extras as JE

-----------------------------------------------------------------

type AsReactor cmd =
    ( AsFacet [cmd] cmd -- implicity required by 'MonadCodify'
    , AsFacet (ReactorCmd cmd) cmd
    )

type MonadReactor p s cmd m =
    ( AsReactor cmd
    , MonadReader (Entity p s) m
    , MonadCommand cmd m
    -- , Monoid (m ())
    )

------------------------------------------------------
-- | Make a unique named id
mkReactId :: (AsReactor cmd, MonadCommand cmd m)
    => J.JSString -> m ReactId
mkReactId n = delegate $ \fire -> do
    f <- codify fire
    postCmd' $ MkReactId n f

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
mkSubject :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> m (Either a (Subject s))
mkSubject gad s = delegate $ \fire -> do
    f <- codify fire
    let gad' = do
            e <- gad
            delegate $ \fw -> case e of
                Left a -> post . f $ Left a
                Right w -> fw $ Right w
    postCmd' $ MkSubject gad' s (f . Right)

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
mkSubject' :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s () -> s -> m (Subject s)
mkSubject' gad s = delegate $ \fire -> do
    f <- codify fire
    postCmd' $ MkSubject gad s f

-- -- | Add a constructed subject to a parent widget
-- addSubject :: (MonadReactor p ss cmd m)
--     => Widget cmd s s a
--     -> s
--     -> (Subject s -> StateT (Scene ss) ReadIORef ())
--     -> m a
-- addSubject wid s f = mkSubject wid s $ \sbj -> tickScene $ f sbj

-- | Schedule cleanup of the callbacks when the parent widget is rerendered.
cleanupSubject ::
    (MonadState (Scene ss) m)
    => Subject s -> m ()
cleanupSubject sbj =
    let cleanup = prolong sbj
    in _plan._doOnRendered._once %= (*> cleanup)

-- | Rerender the ShimComponent using the current @Entity@ context
rerender :: MonadReactor p s cmd m => m ()
rerender = do
    sbj <- view _subject
    postCmd' $ Rerender sbj

-- | Get the 'Scene' and exec actions, using the current @Entity@ context
getScene :: (MonadReactor p s cmd m) => m (Scene s)
getScene = delegate $ \k -> do
    Entity sbj slf <- ask
    c <- codify (k . view (editSceneModel slf))
    postCmd' $ GetScene sbj c

doTickScene :: (AsFacet [cmd] cmd) => Subject s -> SceneState s () -> ReactorCmd cmd
doTickScene sbj m = TickScene sbj (command_ <$> m)

-- | Update the 'Scene' using the current @Entity@ context
tickScene :: MonadReactor p s cmd m => SceneState s () -> m ()
tickScene m = do
    Entity sbj slf <- ask
    let m' = zoom (editSceneModel slf) m
    postCmd' $ doTickScene sbj m'

-- | Update the 'Scene' using the current @Entity@ context,
-- and also return the next action to execute.
tickSceneThen :: (MonadReactor p s cmd m) => SceneState s (m a) -> m a
tickSceneThen m = do
    Entity sbj slf <- ask
    delegate $ \fire -> do
        let m' = zoom (editSceneModel slf) m
            f n = n >>= fire
        f' <- codify f
        postCmd' $ TickScene sbj (f' <$> m')

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- You probably want to use 'trigger'' since most React callbacks return a 'Notice',
-- except for the "ref" callback, in which case you probably want to use 'trackElemental'.
trigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
trigger ri l n goStrict = delegate $ \goLazy -> do
    Entity sbj _ <- ask
    goLazy' <- codify goLazy
    postCmd' $ MkHandler1 goStrict goLazy' $ \act ->
        -- save the created action handler in the elemental
        let updateElemental = _plan._elementals.at ri %= (Just . addListener . fromMaybe newElemental)
            addListener = _listeners.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
            addAction acts = acts & l %~ (*> act)
        in command' $ doTickScene sbj updateElemental

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
trigger' ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger' ri l n goStrict = trigger ri l n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | A varation of trigger which ignores the event but fires the given arg instead.
trigger_ ::
    ( MonadReactor p s cmd m
    )
    => ReactId
    -> Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> J.JSString
    -> a
    -> m a
trigger_ ri l n a = do
    trigger ri l n (const $ pure ())
    pure a

-- | Register actions to execute after a render.
-- Do not 'postCmd'' 'TickScene' or 'Rerender' otherwise it will go into infinite render loops.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onRendered ::
    MonadReactor p s cmd m
    => Lens' (Tagged "Once" (IO ()), Tagged "Always" (IO ())) (IO ())
    -> m ()
    -> m ()
onRendered l m = do
    sbj <- view _subject
    c <- codify' m
    postCmd' $ MkHandler c $ \act ->
            -- save the rendered action handler in the plan
            let addListener = _plan._doOnRendered.l %= (*> act)
            in command' $ doTickScene sbj addListener

-- | This adds a ReactJS "ref" callback assign the ref into an 'EventTarget'
-- for the elemental in the plan, so that the elemental '_targetRef' can be used.
hdlElementalRef :: MonadReactor p s cmd m => ReactId -> m ()
hdlElementalRef ri = trigger ri _always "ref" (pure . JE.fromJSR) >>= hdlRef
  where
    hdlRef x = do
        sbj <- view _subject
        postCmd' . doTickScene sbj $ (_plan._elementals.ix ri._elementalRef .= x)
