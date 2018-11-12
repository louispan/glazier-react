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
    , ModelState
    , evalIO
    , evalIOThen
    , mkReactId
    , setRender
    , mkSubject
    , mkSubject'
    , withMkSubject
    -- FIXME: Rename bookSubjectCleanup
    , bookSubjectCleanup
    , getModel
    , getElementalRef
    , rerender
    , tickModel
    , tickModelThen
    , domTrigger
    , domTrigger_
    , trigger
    , trigger_
    , onMounted
    , onRendered
    , onNextRendered
    , onTicked
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
import Glazier.React.EventTarget
import Glazier.React.Notice
import Glazier.React.ReactId
import Glazier.React.ReadIORef
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
    )

type ModelState s = StateT s ReadIORef

-- | NB. 'ReactorCmd' is not a functor because of the @Widget cmd@ in 'MkSubject'
data ReactorCmd cmd where
    EvalIO :: IO cmd -> ReactorCmd cmd
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> cmd) -> ReactorCmd cmd
    -- | the the rendering function in a Subject, replace any existing render callback
    SetRender :: Subject s -> Window s () -> ReactorCmd cmd
    -- | Make a fully initialized subject (with ShimCallbacks) from a widget spec and state
    MkSubject :: Widget cmd s s () -> s -> (Subject s -> cmd) -> ReactorCmd cmd
    -- | Keep subject alive until the next rerender
    BookSubjectCleanup :: Subject s -> ReactorCmd cmd
    -- | Generate a list of commands from reading the model.
    GetModel :: Subject s -> (s -> cmd) -> ReactorCmd cmd
    -- Get the event target
    -- If a "ref" callback to update 'elementalRef' has not been added;
    -- then add it, rerender, then return the EventTarget.
    GetElementalRef ::
        Subject s
        -> ReactId
        -> (EventTarget -> cmd)
        -> ReactorCmd cmd
    -- | Rerender a ShimComponent using the given state.
    Rerender :: Subject s -> ReactorCmd cmd
    -- | Update and rerender.
    TickModel :: Subject s -> ModelState s cmd -> ReactorCmd cmd
    -- | Create and register a dom callback
    RegisterDOMListener :: NFData a
        => Subject s
        -> JE.JSRep
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ReactorCmd cmd
    -- | Create and register a react callback
    -- If the callback is for "ref", then an listener to update 'elementalRef' for 'GetEventTarget'
    -- will automatically be added just before the listener in 'RegisterReactListener'.
    RegisterReactListener :: NFData a
        => Subject s
        -> ReactId
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ReactorCmd cmd
    -- | Create and register a callback for the mounted event
    RegisterMountedListener ::
        Subject s
        -> cmd
        -> ReactorCmd cmd
    -- | Create and register a callback for the rendered event
    RegisterRenderedListener ::
        Subject s
        -> cmd
        -> ReactorCmd cmd
    -- | Create and register a callback for the rendered event
    RegisterNextRenderedListener ::
        Subject s
        -> cmd
        -> ReactorCmd cmd
    -- | Create and register a callback for the state updated event
    RegisterTickedListener ::
        Subject s
        -> cmd
        -> ReactorCmd cmd

instance Show (ReactorCmd cmd) where
    showsPrec _ (EvalIO _ ) = showString "EvalIO"
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec _ (MkSubject _ _ _) = showString "MkSubject"
    showsPrec _ (BookSubjectCleanup _) = showString "BookSubjectCleanup"
    showsPrec _ (GetModel _ _) = showString "GetModel"
    showsPrec _ (GetElementalRef _ _ _) = showString "GetElementalRef"
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (TickModel _ _) = showString "TickModel"
    showsPrec _ (RegisterDOMListener _ _ _ _ _) = showString "RegisterDOMListener"
    showsPrec _ (RegisterReactListener _ _ _ _ _) = showString "RegisterReactListener"
    showsPrec _ (RegisterMountedListener _ _) = showString "RegisterMountedListener"
    showsPrec _ (RegisterRenderedListener _ _) = showString "RegisterRenderedListener"
    showsPrec _ (RegisterNextRenderedListener _ _) = showString "RegisterNextRenderedListener"
    showsPrec _ (RegisterTickedListener _ _) = showString "RegisterTickedListener"

------------------------------------------------------
-- | Make a unique named id
mkReactId :: (AsReactor cmd, MonadCommand cmd m)
    => J.JSString -> m ReactId
mkReactId n = delegate $ \fire -> do
    f <- codify fire
    exec' $ MkReactId n f

setRender :: (AsReactor cmd, MonadCommand cmd m)
    => Subject s -> Window s () -> m ()
setRender sbj win = exec' $ SetRender sbj win

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
mkSubject :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> m (Either a (Subject s))
mkSubject wid s = delegate $ \fire -> do
    f <- codify fire
    let wid' = wid >>= (instruct . f . Left)
    exec' $ MkSubject wid' s (f . Right)

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
mkSubject' :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s () -> s -> m (Subject s)
mkSubject' gad s = delegate $ \fire -> do
    f <- codify fire
    exec' $ MkSubject gad s f

-- | Make an initialized 'Subject' for a given model using the given 'Widget'.
withMkSubject :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> (Subject s -> m ()) -> m a
withMkSubject wid s k = delegate $ \fire -> do
    f <- codify fire
    k' <- codify k
    let wid' = wid >>= (instruct . f)
    exec' $ MkSubject wid' s k'

-- -- | Add a constructed subject to a parent widget
-- addSubject :: (MonadReactor p ss cmd m)
--     => Widget cmd s s a
--     -> s
--     -> (Subject s -> StateT (Scene ss) ReadIORef ())
--     -> m a
-- addSubject wid s f = mkSubject wid s $ \sbj -> tickScene $ f sbj

-- | Schedule cleanup of the callbacks when the parent widget is rerendered.
bookSubjectCleanup ::
    (MonadReactor p allS cmd m)
    => Subject s -> m ()
-- LOUISDEBUG
bookSubjectCleanup sbj = pure () -- exec' $ BookSubjectCleanup sbj

-- | Rerender the ShimComponent using the current @Entity@ context
rerender :: (MonadReactor p s cmd m) => m ()
rerender = do
    sbj <- view _subject
    exec' $ Rerender sbj

-- | Get the 'Model' and exec actions, using the current @Entity@ context
getModel :: (MonadReactor p s cmd m) => m s
getModel = delegate $ \k -> do
    Entity sbj slf <- ask
    let k' s = case preview slf s of
            Nothing -> pure ()
            Just s' -> k s'
    c <- codify k'
    exec' $ GetModel sbj c

-- | Get the event target
-- If a "ref" callback to update 'elementalRef' has not been added;
-- then add it, rerender, then return the EventTarget.
getElementalRef :: (MonadReactor p s cmd m) => ReactId -> m EventTarget
getElementalRef ri = delegate $ \k -> do
    sbj <- view _subject
    c <- codify k
    exec' $ GetElementalRef sbj ri c

-- | Run an arbitrary IO. This should only be used for testing
evalIO :: (MonadReactor p s cmd m) => IO cmd -> m ()
evalIO m = exec' $ EvalIO m

evalIOThen :: (MonadReactor p s cmd m) => IO (m a) -> m a
evalIOThen m = do
    delegate $ \fire -> do
        -- f :: n a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> cmd
        f' <- codify f
        exec' $ EvalIO (f' <$> m)

-- | Update the 'Model' using the current @Entity@ context
tickModel :: (MonadReactor p s cmd m) => ModelState s () -> m ()
tickModel m = do
    Entity sbj slf <- ask
    let m' = zoom slf m
    exec' $ TickModel sbj (command_ <$> m')

-- | Update the 'Scene' using the current @Entity@ context,
-- and also return the next action to execute.
tickModelThen :: (Also m a, MonadReactor p s cmd m) => ModelState s (m a) -> m a
tickModelThen m = do
    Entity sbj slf <- ask
    delegate $ \fire -> do
        let m' = getAls <$> zoom slf (Als <$> m)
            -- f :: m a -> m ()
            f n = n >>= fire
        -- f' :: m a -> cmd
        f' <- codify f
        exec' $ TickModel sbj (f' <$> m')

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
    exec' $ RegisterDOMListener sbj j n goStrict goLazy'

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
doTrigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
doTrigger ri n goStrict = delegate $ \goLazy -> do
    Entity sbj _ <- ask
    goLazy' <- codify goLazy
    exec' $ RegisterReactListener sbj ri n goStrict goLazy'

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
trigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger ri n goStrict = doTrigger ri n $ handlesNotice goStrict
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
    doTrigger ri n (const $ pure ())
    pure a

-- | Register actions to execute after a render.
-- It is safe to 'postCmd'' a 'TickScene' or 'Rerender'. These command will not
-- trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidMount'
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onMounted ::
    MonadReactor p s cmd m
    => m a
    -> m a
onMounted m = do
    sbj <- view _subject
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterMountedListener sbj c

-- | Register actions to execute after a render.
-- It is safe to 'postCmd'' a 'TickScene' or 'Rerender'. These command will not
-- trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onRendered ::
    MonadReactor p s cmd m
    => m a
    -> m a
onRendered m = do
    sbj <- view _subject
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterRenderedListener sbj c

onNextRendered ::
    MonadReactor p s cmd m
    => m a -> m a
onNextRendered m = do
    sbj <- view _subject
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterNextRenderedListener sbj c

-- | Register actions to execute after the state has been updated with TickState.
-- It is safe to 'postCmd'' another 'TickScene', another onRendered event will
-- not be generated.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onTicked ::
    MonadReactor p s cmd m
    => m a
    -> m a
onTicked m = do
    sbj <- view _subject
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterTickedListener sbj c
