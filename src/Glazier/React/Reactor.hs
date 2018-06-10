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
    , mkReactId
    , setRender
    , mkSubject
    , mkSubject'
    , withMkSubject
    -- , addSubject
    , cleanupSubject
    , rerender
    , getScene
    , tickScene
    , tickSceneThen
    , domTrigger
    , domTrigger_
    , trigger
    , trigger'
    , trigger_
    , onRendered
    , onElementalRef
    ) where

import Control.Also
import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.Notice
import Glazier.React.ReactId
import Glazier.React.ReadIORef
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Widget
import Glazier.React.Window
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

-- | NB. 'ReactorCmd' is not a functor because of the @Widget cmd@ in 'MkSubject'
data ReactorCmd cmd where
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> cmd) -> ReactorCmd cmd
    -- | the the rendering function in a Subject, replace any existing render callback
    SetRender :: Subject s -> Window s () -> ReactorCmd cmd
    -- | Make a fully initialized subject (with ShimCallbacks) from a widget spec and state
    MkSubject :: Widget cmd s s () -> s -> (Subject s -> cmd) -> ReactorCmd cmd
    -- | Rerender a ShimComponent using the given state.
    Rerender :: Subject s -> ReactorCmd cmd
    -- | Generate a list of commands from reading a scene.
    GetScene :: Subject s -> (Scene s -> cmd) -> ReactorCmd cmd
    -- | Update and rerender a scene.
    TickScene :: Subject s -> SceneState s cmd -> ReactorCmd cmd
    -- | Create and register a dom callback
    RegisterDomListener :: NFData a
        => Subject s
        -> JE.JSRep
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ReactorCmd cmd
    -- | Create and register a react callback
    RegisterReactListener :: NFData a
        => Subject s
        -> ReactId
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ReactorCmd cmd
    -- | Create and register a callback for the rendered event
    RegisterRenderedListener ::
        Subject s
        -> cmd
        -> ReactorCmd cmd

instance Show cmd => Show (ReactorCmd cmd) where
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec _ (MkSubject _ _ _) = showString "MkSubject"
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (GetScene _ _) = showString "GetScene"
    showsPrec _ (TickScene _ _) = showString "TickScene"
    showsPrec _ (RegisterDomListener _ _ _ _ _) = showString "RegisterDomListener"
    showsPrec _ (RegisterReactListener _ _ _ _ _) = showString "RegisterReactListener"
    showsPrec _ (RegisterRenderedListener _ _) = showString "RegisterRenderedListener"

------------------------------------------------------
-- | Make a unique named id
mkReactId :: (AsReactor cmd, MonadCommand cmd m)
    => J.JSString -> m ReactId
mkReactId n = delegate $ \fire -> do
    f <- codify fire
    postCmd' $ MkReactId n f

setRender :: (AsReactor cmd, MonadCommand cmd m)
    => Subject s -> Window s () -> m ()
setRender sbj win = postCmd' $ SetRender sbj win

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
mkSubject :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> m (Either a (Subject s))
mkSubject wid s = delegate $ \fire -> do
    f <- codify fire
    let wid' = wid >>= (post . f . Left)
    postCmd' $ MkSubject wid' s (f . Right)

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
mkSubject' :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s () -> s -> m (Subject s)
mkSubject' gad s = delegate $ \fire -> do
    f <- codify fire
    postCmd' $ MkSubject gad s f

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
withMkSubject :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> (Subject s -> m ()) -> m a
withMkSubject wid s k = delegate $ \fire -> do
    f <- codify fire
    k' <- codify k
    let wid' = wid >>= (post . f)
    postCmd' $ MkSubject wid' s k'

-- -- | Add a constructed subject to a parent widget
-- addSubject :: (MonadReactor p ss cmd m)
--     => Widget cmd s s a
--     -> s
--     -> (Subject s -> StateT (Scene ss) ReadIORef ())
--     -> m a
-- addSubject wid s f = mkSubject wid s $ \sbj -> tickScene $ f sbj

-- | Schedule cleanup of the callbacks when the parent widget is rerendered.
cleanupSubject ::
    (MonadState (Scene t) m)
    => Subject s -> m ()
cleanupSubject sbj =
    let cleanup = prolong sbj
    in _plan._tmpCleanup %= (*> cleanup)

-- | Rerender the ShimComponent using the current @Entity@ context
rerender :: MonadReactor p s cmd m => m ()
rerender = do
    sbj <- view _subject
    postCmd' $ Rerender sbj

-- | Get the 'Scene' and exec actions, using the current @Entity@ context
getScene :: (MonadReactor p s cmd m) => m (Scene s)
getScene = delegate $ \k -> do
    Entity sbj slf <- ask
    let k' s = case preview (editSceneModel slf) s of
            Nothing -> pure ()
            Just s' -> k s'
    c <- codify k'
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
tickSceneThen :: (Also m a, MonadReactor p s cmd m) => SceneState s (m a) -> m a
tickSceneThen m = do
    Entity sbj slf <- ask
    delegate $ \fire -> do
        let m' = getAls <$> zoom (editSceneModel slf) (Als <$> m)
            f n = n >>= fire
        f' <- codify f
        postCmd' $ TickScene sbj (f' <$> m')

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
domTrigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
domTrigger j n goStrict = delegate $ \goLazy -> do
    Entity sbj _ <- ask
    goLazy' <- codify goLazy
    postCmd' $ RegisterDomListener sbj j n goStrict goLazy'

-- | A variation of trigger which ignores the event but fires the given arg instead.
domTrigger_ ::
    ( MonadReactor p s cmd m
    )
    => JE.JSRep
    -> J.JSString
    -> a
    -> m a
domTrigger_ j n a = do
    domTrigger j n (const $ pure ())
    pure a

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- You probably want to use 'trigger'' since most React callbacks return a 'Notice',
-- except for the "ref" callback, in which case you probably want to use 'onElementalRef'.
trigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
trigger ri n goStrict = delegate $ \goLazy -> do
    Entity sbj _ <- ask
    goLazy' <- codify goLazy
    postCmd' $ RegisterReactListener sbj ri n goStrict goLazy'

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
trigger' ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger' ri n goStrict = trigger ri n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | A variation of trigger which ignores the event but fires the given arg instead.
trigger_ ::
    ( MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> a
    -> m a
trigger_ ri n a = do
    trigger ri n (const $ pure ())
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
    => m ()
    -> m ()
onRendered m = do
    sbj <- view _subject
    c <- codify' m
    postCmd' $ RegisterRenderedListener sbj c

-- | This adds a ReactJS "ref" callback assign the ref into an 'EventTarget'
-- for the elemental in the plan, so that the elemental '_targetRef' can be used.
onElementalRef :: MonadReactor p s cmd m => ReactId -> m ()
onElementalRef ri = trigger ri "ref" (pure . JE.fromJSR) >>= hdlRef
  where
    hdlRef x = do
        sbj <- view _subject
        postCmd' . doTickScene sbj $ (_plan._elementals.ix ri._elementalRef .= x)
