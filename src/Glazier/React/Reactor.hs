{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.JSString as J
import GHC.Stack
import Glazier.Benign
import Glazier.Command
#ifdef DEBUGIO
import Glazier.DebugIO
#endif
import Glazier.Logger
import Glazier.React.EventTarget
import Glazier.React.Notice
import Glazier.React.Obj
import Glazier.React.Scene
import Glazier.React.ReactId
-- import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE
import JavaScript.Extras.Aeson.Instances ()

-----------------------------------------------------------------

-- | Without 'HasCallStack'
type AsReactor c =
    ( AsFacet [c] c -- required by 'command_'
    , AsFacet (ReactorCmd c) c
    , AsFacet (Benign IO c) c
    , AsFacet LogLine c
#ifdef DEBUGIO
    , AsDebugIO c
#endif
    )

type SceneState s = StateT s (Benign IO)

type MonadReactor c m =
    ( AsReactor c
    , Logger c m
    )

-- | NB. 'ReactorCmd' is not a functor because of the @Widget c@ in 'MkObj'
data ReactorCmd c where
    -- -- | Evaluate some benign IO
    -- EvalBenignIO :: Benign IO c -> ReactorCmd c
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> c) -> ReactorCmd c
    -- | Set the the rendering function in a Obj, replace any existing render callback
    SetRender :: WeakObj s -> Window s () -> ReactorCmd c
    -- | Make a fully initialized object from a widget and model
    -- MkObj :: Widget c s s () -> J.JSString -> s -> (Obj s -> c) -> ReactorCmd c
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
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    -- showsPrec p (MkObj _ logname _ _) = showParen (p >= 11) $
    --     showString $ "MkObj " <> J.unpack logname
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

-- | Make a unique named id
mkReactId :: (HasCallStack, MonadReactor c m)
    => J.JSString -> m ReactId
mkReactId n = delegatify $ \f ->
    logExec' TRACE callStack $ MkReactId n f

setRender :: (HasCallStack, MonadReactor c m)
    => WeakObj s -> Window s () -> m ()
setRender obj win = logExec' TRACE callStack $ SetRender obj win

-- -- | Make an initialized 'Obj' for a given model using the given 'Widget'.
-- mkObj :: (HasCallStack, MonadReactor c m)
--     => Widget c s s a -> J.JSString -> s -> m (Either a (Obj s))
-- mkObj wid logname s = delegatify $ \f -> do
--     let wid' = wid >>= (instruct . f . Left)
--     logExec' TRACE callStack $ MkObj wid' logname s (f . Right)

-- -- | Make an initialized 'Obj' for a given model using the given 'Widget'.
-- mkObj' :: (HasCallStack, MonadReactor c m)
--     => Widget c s s () -> J.JSString -> s -> m (Obj s)
-- mkObj' wid logname s = delegatify $ \f -> do
--     logExec' TRACE callStack $ MkObj wid logname s f

-- -- | Make an initialized 'Obj' for a given model using the given 'Widget'.
-- withMkObj :: (HasCallStack, MonadReactor c m)
--     => Widget c s s a -> J.JSString -> s -> (Obj s -> m ()) -> m a
-- withMkObj wid logname s k = delegatify $ \f -> do
--     k' <- codify k
--     let wid' = wid >>= (instruct . f)
--     logExec' TRACE callStack $ MkObj wid' logname s k'

-- | Rerender the ShimComponent using the current @Entity@ context
-- Consider using 'Control.Monad.Bind.bind1' to bind the 'WeakObj' arg
rerender :: (HasCallStack, MonadReactor c m, GetWeakObj s obj) => obj -> m ()
rerender obj = logExec' TRACE callStack $ ScheduleRerender (obj ^. _getWeakObj)

-- self_rerender :: forall o c r m. (HasCallStack, MonadGadget s c r m) => m ()
-- self_rerender = view _weakObj >>= rerender

-- | Get the 'Model' using the given 'WeakObj'
-- Use 'magnify' to get parts of the model.
-- If magnifying the model with a 'Traversal' you'll need to
-- `Control.Monad.Delegate.fireJust' the 'Maybe s' to get the just the 's'.
-- Consider using 'Control.Monad.Bind.bind1' to bind the 'WeakObj' arg.
-- class GetModel s obj m | obj -> s where
getModel :: (HasCallStack, MonadReactor c m, ToObj s obj) => obj -> m s
getModel obj = delegate $ \fire -> do
    -- let fire' s = case s of
    --         Nothing -> pure ()
    --         Just s' -> fire s'
    -- c <- codify fire'
    -- let c' :: Benign IO s
    --     c' = c <$> maybeModel
    ms <- invoke (id @(Benign IO _) maybeModel)
    case ms of
        Nothing -> pure ()
        Just s' -> fire s'
  where
    maybeModel = runMaybeT $ do
        obj' <- MaybeT $ toObj obj
        lift $ model <$> (benignReadObjScene obj')

-- | Get the event target
-- If a "ref" callback to update 'elementalRef' has not been added;
-- then add it, rerender, then return the EventTarget.
-- Consider using 'Control.Monad.Bind.bind1' to bind the @obj@ arg.
getReactRef :: (HasCallStack, MonadReactor c m, GetWeakObj o obj) => obj -> ReactId -> m EventTarget
getReactRef obj k = delegatify $ \f -> logExec' TRACE callStack $ GetReactRef (obj ^. _getWeakObj) k f

-- | mutates the model for the weakObj.
-- Use 'zoom' to mutate parts of the model.
-- Consider using 'Control.Monad.Bind.bind1' to bind the @obj@ and 'ReactId' arg
mutate :: (HasCallStack, MonadReactor c m, GetWeakObj s obj) => obj -> ReactId -> SceneState s () -> m ()
mutate obj k m = logExec' TRACE callStack $ Mutate (obj ^. _getWeakObj) k (command_ <$> m)

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
-- If zooming the model with a 'Traversal' you'll need to
-- `Control.Lens.Misc.zoomUnder' with 'Control.Also.Als' to make @m a@ a monoid.
-- Consider using 'Control.Monad.Bind.bind1' to bind the @obj@ and 'ReactId' arg
mutateThen :: (HasCallStack, MonadReactor c m, GetWeakObj s obj) => obj -> ReactId -> SceneState s (m a) -> m a
mutateThen obj k m = do
    delegate $ \fire -> do
        -- f :: m a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> c
        f' <- codify f
        logExec' TRACE callStack $ Mutate (obj ^. _getWeakObj) k (f' <$> m)

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- Contrast with 'trigger' which expects a 'Notice'.
domTrigger ::
    ( HasCallStack
    , NFData a
    , MonadReactor c m
    , GetWeakObj s obj
    )
    => obj
    -> JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
domTrigger obj j n goStrict = delegatify $ \f ->
    logExec' TRACE callStack $ RegisterDOMListener (obj ^. _getWeakObj) j n goStrict f

-- | A variation of trigger which ignores the event but fires the given arg instead.
domTrigger_ ::
    (HasCallStack, MonadReactor c m, GetWeakObj s obj)
    => obj
    -> JE.JSRep
    -> J.JSString
    -> a
    -> m a
domTrigger_ obj j n a = do
    domTrigger (obj ^. _getWeakObj) j n (const $ pure ())
    pure a

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- Consider using the other callbacks because all react listeners result in 'Notice'.
trigger' ::
    ( HasCallStack
    , NFData a
    , MonadReactor c m
    , GetWeakObj s obj
    )
    => obj
    -> ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
trigger' obj k n goStrict = delegatify $ \f ->
    logExec' TRACE callStack $ RegisterReactListener (obj ^. _getWeakObj) k n goStrict f

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
-- Contrast with 'domTrigger' which expects a 'JE.JSRep'.
trigger ::
    ( HasCallStack
    , NFData a
    , MonadReactor c m
    , GetWeakObj s obj
    )
    => obj
    -> ReactId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger obj k n goStrict = trigger' obj k n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice g j = MaybeT (pure $ JE.fromJSR j) >>= g

-- | A variation of 'trigger' which ignores the event but fires the given arg instead.
trigger_ ::
    ( HasCallStack
    , MonadReactor c m
    , GetWeakObj s obj
    )
    => obj
    -> ReactId
    -> J.JSString
    -> a
    -> m a
trigger_ obj k n a = do
    -- using trigger' to bypass conversion to 'Notice' in 'trigger'
    trigger' obj k n (const $ pure ())
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
    ( HasCallStack
    , MonadReactor c m
    , GetWeakObj s obj
    )
    => obj
    -> m a
    -> m a
onMounted obj m = delegate $ \fire -> do
    c <- codify' (m >>= fire)
    logExec' TRACE callStack $ RegisterMountedListener (obj ^. _getWeakObj) c

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
    ( HasCallStack
    , MonadReactor c m
    , GetWeakObj s obj
    )
    => obj
    -> m a
    -> m a
onRendered obj m = delegate $ \fire -> do
    f <- codify' (m >>= fire)
    logExec' TRACE callStack $ RegisterRenderedListener (obj ^. _getWeakObj) f

-- | Same as 'onRendered' but the action only occurs once on the next rerender.
onRenderedOnce ::
    ( HasCallStack
    , MonadReactor c m
    , GetWeakObj s obj
    )
    => obj -> m a -> m a
onRenderedOnce obj m = delegate $ \fire -> do
    f <- codify' (m >>= fire)
    logExec' TRACE callStack $ RegisterRenderedOnceListener (obj ^. _getWeakObj) f

-- | Register actions to execute after the state has been updated with 'mutate'.
-- The callback is called` with the 'ReactId' that was passed into 'mutate'.
-- To prevent infinite loops, if this 'onMutated' causes another 'mutate'
-- with the same 'ReactId', then another 'onMutated' callback will NOT be fired.
onMutated ::
    ( HasCallStack
    , MonadReactor c m
    , GetWeakObj s obj
    )
    => obj -> (ReactId -> m a) -> m a
onMutated obj f = delegate $ \fire -> do
    g <- codify ((fire =<<) . f)
    logExec' TRACE callStack $ RegisterMutatedListener (obj ^. _getWeakObj) g

-- -- | Variation of 'onMutated' where you don't care about the 'ReactId' that caused the rerender.
-- onMutated' ::
--     ( HasCallStack
--     , MonadReactor c m
--     , GetWeakObj s obj
--     )
--     => obj -> m a -> m a
-- onMutated' obj f = onMutated obj (const f)

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
