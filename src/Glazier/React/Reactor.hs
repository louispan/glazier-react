-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor where

import Control.Also
import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Benign
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.JSString as J
import qualified Data.List.NonEmpty as NE
import GHC.Stack
import Glazier.Command
#ifdef DEBUGIO
import Glazier.DebugIO
#endif
import Glazier.Logger
import Glazier.React.EventTarget
import Glazier.React.Notice
import Glazier.React.Obj
import Glazier.React.ReactId
import Glazier.React.Scene
import Glazier.React.Window
import qualified JavaScript.Extras as JE

-----------------------------------------------------------------

-- | Without 'HasCallStack'
type AsReactor c =
    ( AsFacet [c] c -- required by 'command_'
    , AsFacet LogLine c
    , AsFacet (Benign IO c) c
#ifdef DEBUGIO
    , AsDebugIO c
#endif
    , AsFacet (ReactorCmd c) c
    )

type SceneState s = StateT s (Benign IO)

type MonadGadget' c s m = (Alternative m, MonadCommand c m, AskLogLevel m, AskWeakObj s m, Also () m)

type MonadGadget c s m = (MonadGadget' c s m, AskReactId m)

type MonadWidget c s m = (MonadGadget' c s m, PutReactId m, PutWindow s m)

-- | 'Widget' is an instance of 'MonadWidget'
type Widget c s =
    ReaderT (WeakObj s) -- 'AskWeakObj'
    (MaybeT -- 'Alternative'
    (ContT () -- 'MonadDelegate'
    (StateT (ReactId) -- 'PutReactId'
    (StateT (Window s ()) -- 'PutWindow'
    (Program c))))) -- 'MonadComand'

-- | Like 'Control.Monad.IO.Unlift.UnliftIO', newtype wrapper prevents impredicative types.
newtype UniftWidget c s m = UniftWidget { unliftWidget :: forall a. m a -> Widget c s a }

-- | Like 'Control.Monad.IO.Unlift.MonadUnliftIO', limits transformers stack of 'ReaderT'
-- and 'IdentityT' on top of @Widget c s m@
class Monad m => MonadUnliftWidget c s m | m -> c s where
    askUnliftWidget :: m (UniftWidget c s m)

instance MonadUnliftWidget c s (Widget c s) where
    askUnliftWidget = pure (UniftWidget id)

instance MonadUnliftWidget c s m => MonadUnliftWidget c s (ReaderT r m) where
    askUnliftWidget = ReaderT $ \r ->
        (\u -> UniftWidget (unliftWidget u . flip runReaderT r)) <$> askUnliftWidget

instance MonadUnliftWidget c s m => MonadUnliftWidget c s (IdentityT m) where
    askUnliftWidget = IdentityT $
        (\u -> UniftWidget (unliftWidget u . runIdentityT)) <$> askUnliftWidget

-- | NB. 'ReactorCmd' is not a functor because of the @Widget c@ in 'MkObj'
data ReactorCmd c where
    -- -- | Evaluate some benign IO
    -- EvalBenignIO :: Benign IO c -> ReactorCmd c
    -- | Make a unique named id
    MkReactId :: NE.NonEmpty J.JSString -> (ReactId -> c) -> ReactorCmd c
    -- | Make a fully initialized object from a widget and model
    -- MkObj :: Widget c s s () -> J.JSString -> s -> (Obj s -> c) -> ReactorCmd c
    MkObj :: Widget c s () -> NE.NonEmpty J.JSString -> s -> (Obj s -> c) -> ReactorCmd c
    -- | Set the the rendering function in a Obj, replace any existing render callback
    SetRender :: WeakObj s -> Window s () -> ReactorCmd c
    -- Get the event target
    -- If a "ref" callback to update 'elementalRef' has not been added;
    -- then add it, rerender, then return the EventTarget.
    GetReactRef ::
        WeakObj s
        -> ReactId
        -> (EventTarget -> c)
        -> ReactorCmd c
    -- | Schedules a 'RerenderNow' to after all the current commands are processed.
    -- Does nothing if already scheduled, or in the middle of a mutation
    -- (ie 'mutations' is non empty)
    ScheduleRerender :: WeakObj s -> ReactorCmd c
    -- | Private effect used by executor:
    -- Rerender now
    -- Does nothing if rerender has not been scheduled or
    -- if in the middle of mutation ('mutations' is not empty)
    RerenderNow :: WeakObj s -> ReactorCmd c
    -- | Update, store 'ReactId' in 'mutataions' and fire 'mutatedListener'.
    -- The 'mutatedListener' may cause another 'Mutate' which will fire another
    -- 'mutatedListener' as long as the 'ReactId' has not been seen before.
    -- When the cycle of 'Mutate' and 'mutatedListener' finishes, the 'mutations' is
    -- cleared.
    -- Finally 'Rerender' is called.
    Mutate :: WeakObj s -> ReactId -> SceneState s c -> ReactorCmd c
    -- | Private effect used by executor: Calls the model mutatedListener with 'ReactId'
    -- that caused the mutation.
    -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    NotifyMutated :: WeakObj s -> ReactId -> ReactorCmd c
    -- | Private effect used by executor: Resets the mutated state for this ReactId
    -- If there are no more pending mutations, then rerender.
    -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    ResetMutation :: WeakObj s -> ReactId -> ReactorCmd c
    -- | Create and register a dom callback
    RegisterDOMListener :: NFData a
        => WeakObj s
        -> JE.JSRep
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> c)
        -> ReactorCmd c
    -- | Create and register a react callback
    -- If the callback is for "ref", then an listener to update 'elementalRef' for 'GetEventTarget'
    -- will automatically be added just before the listener in 'RegisterReactListener'.
    RegisterReactListener :: NFData a
        => WeakObj s
        -> ReactId
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> c)
        -> ReactorCmd c
    -- | Create and register a callback for the mounted event
    RegisterMountedListener ::
        WeakObj s
        -> c
        -> ReactorCmd c
    -- | Create and register a callback for the rendered event
    RegisterRenderedListener ::
        WeakObj s
        -> c
        -> ReactorCmd c
    -- | Create and register a callback for the rendered event
    RegisterRenderedOnceListener ::
        WeakObj s
        -> c
        -> ReactorCmd c
    -- | Create and register a callback for the state updated event
    RegisterMutatedListener ::
        WeakObj s
        -> (ReactId -> c)
        -> ReactorCmd c

-- FIXME: can show ReactId and caption
instance Show (ReactorCmd c) where
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec p (MkObj _ logname _ _) = showParen (p >= 11) $
        showString $ "MkObj " <> (show logname)
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec _ (GetReactRef _ _ _) = showString "GetReactRef"
    showsPrec _ (ScheduleRerender _) = showString "ScheduleRerender"
    showsPrec _ (RerenderNow _) = showString "RerenderNow"
    showsPrec p (Mutate _ k _) = showParen (p >= 11) $
        showString "Mutate " . shows k
    showsPrec p (NotifyMutated _ k) = showParen (p >= 11) $
        showString "NotifyMutated " . shows k
    showsPrec p (ResetMutation _ k) = showParen (p >= 11) $
        showString "ResetMutation " . shows k
    showsPrec p (RegisterDOMListener _ k n _ _) = showParen (p >= 11) $
        showString "RegisterDOMListener " . shows k . showString " " . shows n
    showsPrec p (RegisterReactListener _ k n _ _) = showParen (p >= 11) $
        showString "RegisterReactListener " . shows k . showString " " . shows n
    showsPrec _ (RegisterMountedListener _ _) = showString "RegisterMountedListener"
    showsPrec _ (RegisterRenderedListener _ _) = showString "RegisterRenderedListener"
    showsPrec _ (RegisterRenderedOnceListener _ _) = showString "RegisterRenderedOnceListener"
    showsPrec _ (RegisterMutatedListener _ _) = showString "RegisterMutatedListener"


------------------------------------------------------
-- Basic
------------------------------------------------------

-- | Make a unique named id
mkReactId :: (HasCallStack, AsReactor c, MonadCommand c m, AskLogLevel m)
    => NE.NonEmpty J.JSString -> m ReactId
mkReactId n = delegatify $ \f ->
    logExec' TRACE callStack $ MkReactId n f

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj :: (HasCallStack, AsReactor c, MonadCommand c m, AskLogLevel m)
    => Widget c s a -> NE.NonEmpty J.JSString -> s -> m (Either a (Obj s))
mkObj wid logname s = delegatify $ \f -> do
    let wid' = wid >>= (instruct . f . Left)
    logExec' TRACE callStack $ MkObj wid' logname s (f . Right)

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
-- Unlike 'unliftMkObj'', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj' :: (HasCallStack, AsReactor c, MonadCommand c m, AskLogLevel m)
    => Widget c s () -> NE.NonEmpty J.JSString -> s -> m (Obj s)
mkObj' wid logname s = delegatify $ \f -> do
    logExec' TRACE callStack $ MkObj wid logname s f

------------------------------------------------------
-- MonadUnliftWidget
------------------------------------------------------

-- | Convenient variation of 'mkObj' where the widget is unlifted from the given monad.
-- This is useful for transformer stacks that require addition MonadReader-like effects.
unliftMkObj :: (HasCallStack, AsReactor c, MonadCommand c m, AskLogLevel m, MonadUnliftWidget c s m)
    => m a -> NE.NonEmpty J.JSString -> s -> m (Either a (Obj s))
unliftMkObj m logname s = do
    u <- askUnliftWidget
    mkObj (unliftWidget u m) logname s

-- | Convenient variation of 'mkObj'' where the widget is unlifted from the given monad.
-- This is useful for transformer stacks that require addition MonadReader-like effects.
unliftMkObj' :: (HasCallStack, AsReactor c, MonadCommand c m, AskLogLevel m, MonadUnliftWidget c s m)
    => m () -> NE.NonEmpty J.JSString -> s -> m (Obj s)
unliftMkObj' m logname s = do
    u <- askUnliftWidget
    mkObj' (unliftWidget u m) logname s

-- -- | A variation of 'mkObj', with the @Right (Obj s)@ processed in a separate handler.
-- withMkObj :: (HasCallStack, AsReactor c, MonadCommand c m, AskLogLevel m, MonadUnliftWidget c s m)
--     => m a -> J.JSString -> o -> (Obj o -> m ()) -> m a
-- withMkObj m logname s k = do
--     e <- mkObj m logname s
--     delegate $ \fire ->
--         case e of
--             Left a -> fire a
--             Right o -> k o

---------------------------
-- MonadGadget' c s m
---------------------------

-- -- | Private function: should only be called by Exec
-- setRender :: (HasCallStack, AsReactor c, MonadGadget' c s m)
--     => Window s () -> m ()
-- setRender win = do
--     obj <- askWeakObj
--     logExec' TRACE callStack $ SetRender obj win


-- | Rerender the ShimComponent using the current @Entity@ context
-- Consider using 'Control.Monad.Bind.bind1' to bind the 'WeakObj' arg
rerender :: (HasCallStack, AsReactor c, MonadGadget' c s m) => m ()
rerender = do
    obj <- askWeakObj
    logExec' TRACE callStack $ ScheduleRerender obj


-- | Get the 'Model' using the given 'WeakObj'
-- Use 'magnify' to get parts of the model.
-- If magnifying the model with a 'Traversal' you'll need to
-- `Control.Monad.Delegate.onJust' the 'Maybe s' to get the just the 's'.
-- Consider using 'Control.Monad.Bind.bind1' to bind the 'WeakObj' arg.
-- class GetModel s obj m | obj -> s where
getModel :: (HasCallStack, AsReactor c, MonadGadget' c s m) => Traversal' s s' -> m s'
getModel sbj = do
    obj <- askWeakObj
    ms <- logInvoke TRACE callStack (id @(Benign IO _) (go <$> benignReadWeakObjScene obj))
    onJust ms
  where
    go scn = scn ^? (_Just._model.sbj)

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- Contrast with 'trigger' which expects a 'Notice'.
domTrigger ::
    (HasCallStack, NFData a, AsReactor c, MonadGadget' c s m)
    => JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
domTrigger j n goStrict = do
    obj <- askWeakObj
    delegatify $ \f ->
        logExec' TRACE callStack $ RegisterDOMListener obj j n goStrict f

-- | A variation of trigger which ignores the event but fires the given arg instead.
-- Unlike 'domTrigger' the @a@ does not need to be @NFData a@
domTrigger_ ::
    (HasCallStack, AsReactor c, MonadGadget' c s m)
    => JE.JSRep
    -> J.JSString
    -> a
    -> m a
domTrigger_ j n a = do
    domTrigger j n (const $ pure ())
    pure a

-- | Register actions to execute after a render.
-- It is safe to 'exec'' a 'Mutate' or 'Rerender' in this callback.
-- These command will not trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidMount'
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onMounted ::
    (HasCallStack, AsReactor c, MonadGadget' c s m)
    => m a
    -> m a
onMounted m = do
    obj <- askWeakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        logExec' TRACE callStack $ RegisterMountedListener obj c

-- | Register actions to execute after a render.
-- It is safe to 'exec'' a 'Mutate' or 'Rerender'. These command will not
-- trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onRendered ::
    (HasCallStack, AsReactor c, MonadGadget' c s m)
    => m a
    -> m a
onRendered m = do
    obj <- askWeakObj
    delegate $ \fire -> do
        f <- codify' (m >>= fire)
        logExec' TRACE callStack $ RegisterRenderedListener obj f

-- | Same as 'onRendered' but the action only occurs once on the next rerender.
onRenderedOnce ::
    (HasCallStack, AsReactor c, MonadGadget' c s m)
    => m a -> m a
onRenderedOnce m = do
    obj <- askWeakObj
    delegate $ \fire -> do
        f <- codify' (m >>= fire)
        logExec' TRACE callStack $ RegisterRenderedOnceListener obj f

-- | Register actions to execute after the state has been updated with 'mutate'.
-- The callback is called` with the 'ReactId' that was passed into 'mutate'.
-- To prevent infinite loops, if this 'onMutated' causes another 'mutate'
-- with the same 'ReactId', then another 'onMutated' callback will NOT be fired.
onMutated ::
    (HasCallStack, AsReactor c, MonadGadget' c s m)
    => (ReactId -> m a) -> m a
onMutated f = do
    obj <- askWeakObj
    delegate $ \fire -> do
        g <- codify ((fire =<<) . f)
        logExec' TRACE callStack $ RegisterMutatedListener obj g

-- -- | Variation of 'onMutated' where you don't care about the 'ReactId' that caused the rerender.
-- onMutated' ::
--     (HasCallStack, MonadGadget' c s m)
--     => obj -> m a -> m a
-- onMutated' f = onMutated (const f)

---------------------------
-- MonadGadget c s m
---------------------------

-- | Get the event target
-- If a "ref" callback to update 'reactRef' has not been added;
-- then add it, rerender, then return the EventTarget.
getReactRef :: (HasCallStack, AsReactor c, MonadGadget c s m) => m EventTarget
getReactRef = do
    obj <- askWeakObj
    k <- askReactId
    delegatify $ \f -> logExec' TRACE callStack $ GetReactRef obj k f

-- | mutates the model for the weakObj.
-- Use 'zoom' to mutate parts of the model.
mutate :: (HasCallStack, AsReactor c, MonadGadget c s m) => Traversal' s s' -> SceneState s' () -> m ()
mutate sbj m = do
    obj <- askWeakObj
    k <- askReactId
    logExec' TRACE callStack $ Mutate obj k (command_ <$> (zoom sbj m))

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
mutateThen :: (Also a m, HasCallStack, AsReactor c, MonadGadget c s m) => Traversal' s s' -> SceneState s' (m a) -> m a
mutateThen sbj m = do
    obj <- askWeakObj
    k <- askReactId
    delegate $ \fire -> do
        -- f :: m a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> c
        f' <- codify f
        logExec' TRACE callStack $ Mutate obj k (f' <$> (zoomUnder (iso Als getAls) sbj m))


-- | Create a callback for a 'JE.JSRep' and add it to this reactant's dlist of listeners.
-- Consider using the other callbacks because all react listeners result in 'Notice'.
trigger' ::
    (HasCallStack, NFData a, AsReactor c, MonadGadget c s m)
    => J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
trigger' n goStrict = do
    obj <- askWeakObj
    k <- askReactId
    delegatify $ \f ->
        logExec' TRACE callStack $ RegisterReactListener obj k n goStrict f

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
-- Contrast with 'domTrigger' which expects a 'JE.JSRep'.
trigger ::
    (HasCallStack, NFData a, AsReactor c, MonadGadget c s m)
    => J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger n goStrict = do
    trigger' n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice g j = MaybeT (pure $ JE.fromJSRep j) >>= g

-- | A variation of 'trigger' which ignores the event but fires the given arg instead.
trigger_ ::
    (HasCallStack, AsReactor c, MonadGadget c s m)
    => J.JSString
    -> a
    -> m a
trigger_ n a = do
    -- using trigger' to bypass conversion to 'Notice' in 'trigger'
    trigger' n (const $ pure ())
    pure a

-- | Orphan instance because it requires AsReactor
-- LOUISFIXME: Think about this, use ReaderT (s -> Either e Obj s)?
-- Use a new typeclass?
-- instance (A.AFromJSON m s, AsReactor c, MonadCommand c m, LogLevelEnv m, MonadTrans t, MonadReader (Widget c s s a, J.JSString) (t m), MonadError a (t m)) => A.AFromJSON (t m) (Obj s) where
--     aparseJSON v = do
--         ms <- fmap lift (A.aparseJSON v)
--         let meobj = do
--                 (wid, logname) <- ask
--                 s <- ms
--                 e <- lift $ mkObj wid logname s
--                 case e of
--                     Left e' -> throwError e'
--                     Right obj -> pure obj
--         pure meobj


----------------------------------------------------------------------------------
