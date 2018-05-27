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
    , ReactorCmd(..)
    , SceneState
    , mkSubject
    -- , addSubject
    , cleanupSubject
    , rerender
    , withScene
    , tickScene
    , tickScene'
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
import Glazier.React.MkId
import Glazier.React.Notice
import Glazier.React.ReadIORef
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

type SceneState s = StateT (Scene s) ReadIORef

-- | NB. ReactorCmd is not a functor.
data ReactorCmd cmd where
    MkSubject :: Widget cmd s s () -> s -> (Subject s -> cmd) -> ReactorCmd cmd
    -- | Rerender a ShimComponent using the given state.
    Rerender :: Subject s -> ReactorCmd cmd
    -- Generate a list of commands from reading a scene.
    WithScene :: Subject s -> (Scene s -> cmd) -> ReactorCmd cmd
    -- Update and rerender a scene.
    TickScene :: Subject s -> SceneState s cmd -> ReactorCmd cmd
    -- DoReadIORef :: IORef a -> (a -> cmd) -> ReactorCmd
    MkHandler :: cmd -> (IO () -> cmd) -> ReactorCmd cmd
    -- | Convert a callback to a @JE.JSRep -> IO ()@
    MkHandler1 :: NFData a
        => (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ((JE.JSRep -> IO ()) -> cmd)
        -> ReactorCmd cmd

instance Show cmd => Show (ReactorCmd cmd) where
    showsPrec _ (MkSubject _ _ _) = showString "MkSubject"
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (WithScene _ _) = showString "WithScene"
    showsPrec _ (TickScene _ _) = showString "TickScene"
    showsPrec p (MkHandler c _) = showParen (p >= 11) $
        showString "MkHandler " . shows c
    showsPrec _ (MkHandler1 _ _ _) = showString "MkHandler1"

------------------------------------------------------

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
mkSubject :: (AsReactor cmd, MonadCommand cmd m) => Widget cmd s s a -> s -> (Subject s -> m ()) -> m a
mkSubject (Widget win gad) s k =
    -- given a handler for @a@
    delegate $ \fire -> do
        f' <- codifies fire
        -- convert the gadget to also handle @a@
        let gad' = gad >>= (posts . f')
        k' <- codify k
        postCmd' $ MkSubject (Widget win gad') s k'

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
withScene :: (MonadReactor p s cmd m) => (Scene s -> m ()) -> m ()
withScene go = do
    Entity sbj slf <- ask
    let go' s = case preview (editSceneModel slf) s of
            Nothing -> pure ()
            Just s' -> go s'
    c <- codify go'
    postCmd' $ WithScene sbj c

-- | Update the 'Scene' using the current @Entity@ context, wheere the update
-- state function returns the next action to execute.
tickScene :: (Monoid (m a), MonadReactor p s cmd m) => SceneState s (m a) -> m a
tickScene m = do
    Entity sbj slf <- ask
    delegate $ \fire -> do
        let m' = zoom (editSceneModel slf) m
            f n = n >>= fire
        f' <- codify f
        postCmd' $ TickScene sbj (f' <$> m')

-- | Update the 'Scene' using the current @Entity@ context
tickScene' :: MonadReactor p s cmd m => SceneState s () -> m ()
tickScene' m = do
    Entity sbj slf <- ask
    let m' = zoom (editSceneModel slf) m
    postCmd' $ TickScene sbj (command_ <$> m')

mkTickScene' :: (AsFacet [cmd] cmd) => Subject s -> SceneState s () -> ReactorCmd cmd
mkTickScene' sbj m = TickScene sbj (command_ <$> m)

-- | Create a callback and add it to this elemental's dlist of listeners.
doTrigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ElementalId
    -> Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> m ())
    -> m ()
doTrigger eid l n goStrict goLazy = do
    Entity sbj _ <- ask
    goLazy' <- codify goLazy
    postCmd' $ MkHandler1 goStrict goLazy' $ \act ->
        -- save the created action handler in the elemental
        let updateElemental = _plan._elementals.at eid %= (Just . addListener . fromMaybe newElemental)
            addListener = _listeners.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
            addAction acts = acts & l %~ (*> act)
        in command' $ mkTickScene' sbj updateElemental

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- You probably want to use 'trigger'' since most React callbacks return a 'Notice',
-- except for the "ref" callback, in which case you probably want to use 'trackElemental'.
trigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ElementalId
    -> Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
trigger eid l n goStrict = delegate $ doTrigger eid l n goStrict

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
trigger' ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ElementalId
    -> Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger' eid l n goStrict = trigger eid l n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | A varation of trigger which ignores the event but fires the given arg instead.
trigger_ ::
    ( MonadReactor p s cmd m
    )
    => ElementalId
    -> Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> J.JSString
    -> a
    -> m a
trigger_ eid l n a = do
    trigger eid l n (const $ pure ())
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
            in command' $ mkTickScene' sbj addListener

-- | This adds a ReactJS "ref" callback assign the ref into an 'EventTarget'
-- for the elemental in the plan, so that the elemental '_targetRef' can be used.
hdlElementalRef :: MonadReactor p s cmd m => ElementalId -> m ()
hdlElementalRef eid = trigger eid _always "ref" (pure . JE.fromJSR) >>= hdlRef
  where
    hdlRef x = do
        sbj <- view _subject
        postCmd' . mkTickScene' sbj $ (_plan._elementals.ix eid._elementalRef .= x)
