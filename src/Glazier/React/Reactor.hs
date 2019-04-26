{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Control.DeepSeq
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Benign
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.JSString as J
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
-- import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE
import JavaScript.Extras.Aeson.Instances ()

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

type MonadGadget' c o m = (MonadCommand c m, LogLevelReader m, WeakObjReader o m)

type MonadGadget c o m = (MonadGadget' c o m, ReactIdReader m)

type MonadWidget' c o s m = (MonadGadget' c o m, MonadError (Window s ()) m)

type MonadWidget c o s m = (MonadWidget' c o s m, ReactIdReader m)

type Widget c o s = ExceptT (Window s ())
    (ReaderT (WeakObj o)
    (ReaderT (ReactId)
    (ContT ()
    (Program c))))

-- | Like 'Control.Monad.IO.Unlift.UnliftIO', newtype wrapper prevents impredicative types.
newtype UniftWidget c o s m = UniftWidget { unliftWidget :: forall a. m a -> Widget c o s a }

-- | Like 'Control.Monad.IO.Unlift.MonadUnliftIO', limits transformers stack of 'ReaderT'
-- and 'IdentityT' on top of @Widget c o m@
class Monad m => MonadUnliftWidget c o s m | m -> c o where
    askUnliftWidget :: m (UniftWidget c o s m)

instance MonadUnliftWidget c o s (Widget c o s) where
    askUnliftWidget = pure (UniftWidget id)

instance MonadUnliftWidget c o s m => MonadUnliftWidget c o s (ReaderT r m) where
    askUnliftWidget = ReaderT $ \r ->
        (\u -> UniftWidget (unliftWidget u . flip runReaderT r)) <$> askUnliftWidget

instance MonadUnliftWidget c o s m => MonadUnliftWidget c o s (IdentityT m) where
    askUnliftWidget = IdentityT $
        (\u -> UniftWidget (unliftWidget u . runIdentityT)) <$> askUnliftWidget

-- | NB. 'ReactorCmd' is not a functor because of the @Widget c@ in 'MkObj'
data ReactorCmd c where
    -- -- | Evaluate some benign IO
    -- EvalBenignIO :: Benign IO c -> ReactorCmd c
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> c) -> ReactorCmd c
    -- | Make a fully initialized object from a widget and model
    -- MkObj :: Widget c s s () -> J.JSString -> s -> (Obj s -> c) -> ReactorCmd c
    MkObj :: Widget c o o () -> J.JSString -> o -> (Obj o -> c) -> ReactorCmd c
    -- | Set the the rendering function in a Obj, replace any existing render callback
    SetRender :: WeakObj o -> Window o () -> ReactorCmd c
    -- Get the event target
    -- If a "ref" callback to update 'elementalRef' has not been added;
    -- then add it, rerender, then return the EventTarget.
    GetReactRef ::
        WeakObj so
        -> ReactId
        -> (EventTarget -> c)
        -> ReactorCmd c
    -- | Schedules a 'RerenderNow' to after all the current commands are processed.
    -- Does nothing if already scheduled, or in the middle of a mutation
    -- (ie 'mutations' is non empty)
    ScheduleRerender :: WeakObj o -> ReactorCmd c
    -- | Private effect used by executor:
    -- Rerender now
    -- Does nothing if rerender has not been scheduled or
    -- if in the middle of mutation ('mutations' is not empty)
    RerenderNow :: WeakObj o -> ReactorCmd c
    -- | Update, store 'ReactId' in 'mutataions' and fire 'mutatedListener'.
    -- The 'mutatedListener' may cause another 'Mutate' which will fire another
    -- 'mutatedListener' as long as the 'ReactId' has not been seen before.
    -- When the cycle of 'Mutate' and 'mutatedListener' finishes, the 'mutations' is
    -- cleared.
    -- Finally 'Rerender' is called.
    Mutate :: WeakObj o -> ReactId -> SceneState o c -> ReactorCmd c
    -- | Private effect used by executor: Calls the model mutatedListener with 'ReactId'
    -- that caused the mutation.
    -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    NotifyMutated :: WeakObj o -> ReactId -> ReactorCmd c
    -- | Private effect used by executor: Resets the mutated state for this ReactId
    -- If there are no more pending mutations, then rerender.
    -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    ResetMutation :: WeakObj o -> ReactId -> ReactorCmd c
    -- | Create and register a dom callback
    RegisterDOMListener :: NFData a
        => WeakObj o
        -> JE.JSRep
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> c)
        -> ReactorCmd c
    -- | Create and register a react callback
    -- If the callback is for "ref", then an listener to update 'elementalRef' for 'GetEventTarget'
    -- will automatically be added just before the listener in 'RegisterReactListener'.
    RegisterReactListener :: NFData a
        => WeakObj o
        -> ReactId
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> c)
        -> ReactorCmd c
    -- | Create and register a callback for the mounted event
    RegisterMountedListener ::
        WeakObj o
        -> c
        -> ReactorCmd c
    -- | Create and register a callback for the rendered event
    RegisterRenderedListener ::
        WeakObj o
        -> c
        -> ReactorCmd c
    -- | Create and register a callback for the rendered event
    RegisterRenderedOnceListener ::
        WeakObj o
        -> c
        -> ReactorCmd c
    -- | Create and register a callback for the state updated event
    RegisterMutatedListener ::
        WeakObj o
        -> (ReactId -> c)
        -> ReactorCmd c

-- FIXME: can show ReactId and caption
instance Show (ReactorCmd c) where
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec p (MkObj _ logname _ _) = showParen (p >= 11) $
        showString $ "MkObj " <> J.unpack logname
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
mkReactId :: (HasCallStack, AsReactor c, MonadCommand c m, LogLevelReader m)
    => J.JSString -> m ReactId
mkReactId n = delegatify $ \f ->
    logExec' TRACE callStack $ MkReactId n f

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj :: (HasCallStack, AsReactor c, MonadCommand c m, LogLevelReader m)
    => Widget c o o a -> J.JSString -> o -> m (Either a (Obj o))
mkObj wid logname o = delegatify $ \f -> do
    let wid' = wid >>= (instruct . f . Left)
    logExec' TRACE callStack $ MkObj wid' logname o (f . Right)

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
-- Unlike 'unliftMkObj'', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj' :: (HasCallStack, AsReactor c, MonadCommand c m, LogLevelReader m)
    => Widget c o o () -> J.JSString -> o -> m (Obj o)
mkObj' wid logname o = delegatify $ \f -> do
    logExec' TRACE callStack $ MkObj wid logname o f

------------------------------------------------------
-- MonadUnliftWidget
------------------------------------------------------

-- | Convenient variation of 'mkObj' where the widget is unlifted from the given monad.
unliftMkObj :: (HasCallStack, AsReactor c, MonadCommand c m, LogLevelReader m, MonadUnliftWidget c o o m)
    => m a -> J.JSString -> o -> m (Either a (Obj o))
unliftMkObj m logname o = do
    u <- askUnliftWidget
    mkObj (unliftWidget u m) logname o

-- | Convenient variation of 'mkObj'' where the widget is unlifted from the given monad.
unliftMkObj' :: (HasCallStack, AsReactor c, MonadCommand c m, LogLevelReader m, MonadUnliftWidget c o o m)
    => m () -> J.JSString -> o -> m (Obj o)
unliftMkObj' m logname o = do
    u <- askUnliftWidget
    mkObj' (unliftWidget u m) logname o

-- -- | A variation of 'mkObj', with the @Right (Obj s)@ processed in a separate handler.
-- withMkObj :: (HasCallStack, AsReactor c, MonadCommand c m, LogLevelReader m, MonadUnliftWidget c o m)
--     => m a -> J.JSString -> o -> (Obj o -> m ()) -> m a
-- withMkObj m logname s k = do
--     e <- mkObj m logname s
--     delegate $ \fire ->
--         case e of
--             Left a -> fire a
--             Right o -> k o

---------------------------
-- MonadGadget' c o m
---------------------------

setRender :: (HasCallStack, AsReactor c, MonadGadget' c o m)
    => Window o () -> m ()
setRender win = do
    obj <- askWeakObj
    logExec' TRACE callStack $ SetRender obj win


-- | Rerender the ShimComponent using the current @Entity@ context
-- Consider using 'Control.Monad.Bind.bind1' to bind the 'WeakObj' arg
rerender :: (HasCallStack, AsReactor c, MonadGadget' c o m) => m ()
rerender = do
    obj <- askWeakObj
    logExec' TRACE callStack $ ScheduleRerender obj


-- | Get the 'Model' using the given 'WeakObj'
-- Use 'magnify' to get parts of the model.
-- If magnifying the model with a 'Traversal' you'll need to
-- `Control.Monad.Delegate.fireJust' the 'Maybe s' to get the just the 's'.
-- Consider using 'Control.Monad.Bind.bind1' to bind the 'WeakObj' arg.
-- class GetModel s obj m | obj -> s where
getModel :: (HasCallStack, AsReactor c, MonadGadget' c o m) => Traversal' o s -> m s
getModel sbj = do
    obj <- askWeakObj
    ms <- runMaybeT $ do
        o <- MaybeT $ invoke (id @(Benign IO _) (fmap model <$> benignReadWeakObjScene obj))
        MaybeT $ pure $ preview sbj o
    fireJust ms

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- Contrast with 'trigger' which expects a 'Notice'.
domTrigger ::
    (HasCallStack, NFData a, AsReactor c, MonadGadget' c o m)
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
    (HasCallStack, AsReactor c, MonadGadget' c o m)
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
    (HasCallStack, AsReactor c, MonadGadget' c o m)
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
    (HasCallStack, AsReactor c, MonadGadget' c o m)
    => m a
    -> m a
onRendered m = do
    obj <- askWeakObj
    delegate $ \fire -> do
        f <- codify' (m >>= fire)
        logExec' TRACE callStack $ RegisterRenderedListener obj f

-- | Same as 'onRendered' but the action only occurs once on the next rerender.
onRenderedOnce ::
    (HasCallStack, AsReactor c, MonadGadget' c o m)
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
    (HasCallStack, AsReactor c, MonadGadget' c o m)
    => (ReactId -> m a) -> m a
onMutated f = do
    obj <- askWeakObj
    delegate $ \fire -> do
        g <- codify ((fire =<<) . f)
        logExec' TRACE callStack $ RegisterMutatedListener obj g

-- -- | Variation of 'onMutated' where you don't care about the 'ReactId' that caused the rerender.
-- onMutated' ::
--     (HasCallStack, MonadGadget' c o m)
--     => obj -> m a -> m a
-- onMutated' f = onMutated (const f)

---------------------------
-- MonadGadget c o m
---------------------------

-- | Get the event target
-- If a "ref" callback to update 'elementalRef' has not been added;
-- then add it, rerender, then return the EventTarget.
-- Consider using 'Control.Monad.Bind.bind1' to bind the @obj@ arg.
getReactRef :: (HasCallStack, AsReactor c, MonadGadget c o m) => m EventTarget
getReactRef = do
    obj <- askWeakObj
    k <- askReactId
    delegatify $ \f -> logExec' TRACE callStack $ GetReactRef obj k f

-- | mutates the model for the weakObj.
-- Use 'zoom' to mutate parts of the model.
-- Consider using 'Control.Monad.Bind.bind1' to bind the @obj@ and 'ReactId' arg
mutate :: (HasCallStack, AsReactor c, MonadGadget c o m) => Traversal' o s -> SceneState s () -> m ()
mutate sbj m = do
    obj <- askWeakObj
    k <- askReactId
    logExec' TRACE callStack $ Mutate obj k (command_ <$> (zoom sbj m))

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
mutateThen :: (Also a m, HasCallStack, AsReactor c, MonadGadget c o m) => Traversal' o s -> SceneState s (m a) -> m a
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
    (HasCallStack, NFData a, AsReactor c, MonadGadget c o m)
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
    (HasCallStack, NFData a, AsReactor c, MonadGadget c o m)
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
    (HasCallStack, NFData a, AsReactor c, MonadGadget c o m)
    => J.JSString
    -> a
    -> m a
trigger_ n a = do
    -- using trigger' to bypass conversion to 'Notice' in 'trigger'
    trigger' n (const $ pure ())
    pure a

-- -- -- | Orphan instance because it requires AsReactor
-- -- instance (MonadIO m, A.AToJSON (Benign m) s) => A.AToJSON (Benign m) (Obj s) where
-- --     atoEncoding o = (model <$> benignReadObj o) >>= A.atoEncoding

-- -- -- | Orphan instance because it requires AsReactor
-- -- instance (MonadReactor c m, A.AFromJSON m s) => A.AFromJSON (ExceptT a (ReaderT (Widget c s s a) m)) (Obj s) where
-- --     aparseJSON v = do
-- --         ms <- fmap lift (A.aparseJSON v)
-- --         let meobj = do
-- --                 wid <- ask
-- --                 s <- ms
-- --                 mkObj wid s
-- --         pure (ExceptT meobj)

-- -- -- | Specify a default widget for a model type
-- -- -- to be used in AFromJSON instances.
-- -- -- Use @Tagged t s@ to add other instances, which can be `coerce`d back to @s@.
-- -- class DefaultWidget s where
-- --     defaultWidget :: Widget c s s ()


-- wack :: (AsFacet [c] c, MonadWidget c o o m) => m ()
-- wack = pure ()

-- wock :: AsFacet [c] c => ReaderT (ReactId)
--     (ReaderT (WeakObj o)
--     (ExceptT (Window o ())
--     (ContT ()
--     (Program c)))) ()
-- wock = wack

-- data WackCmd c where
--      WackCmd :: (MonadWidget c o o m) => m () -> WackCmd c

-- wack1 :: (AsFacet [c] c) => WackCmd c
-- wack1 = WackCmd wock

-- -- wack2 :: AsFacet [c] c => WackCmd c -> ReaderT (ReactId)
-- --     (ReaderT (WeakObj o)
-- --     (ExceptT (Window o ())
-- --     (ContT ()
-- --     (Program c)))) c
-- wack2 (WackCmd m) = m


-- wack2 :: forall c o. (AsFacet [c] c) => (forall m. MonadWidget c o o m => m ()) -> ReaderT (ReactId)
--     (ReaderT (WeakObj o)
--     (ExceptT (Window o ())
--     (ContT ()
--     (Program c)))) ()
-- wack2 a = a
