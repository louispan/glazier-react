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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor where

import Control.Also
import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad.Benign
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified Data.List.NonEmpty as NE
import GHC.Stack
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.EventTarget
import Glazier.React.Markup
import Glazier.React.Notice
import Glazier.React.ReactId
import Glazier.React.Model
import Glazier.React.Widget
-- import Glazier.React.Widget.Internal
import qualified JavaScript.Extras as JE

-----------------------------------------------------------------

-- | Without 'HasCallStack'
type AsReactor c =
    ( AsFacet [c] c -- required by 'command_'
    , AsFacet LogLine c
    -- , AsFacet (Benign IO c) c
#ifdef DEBUGIO
    , AsDebugIO c
#endif
    , AsFacet (ReactorCmd c) c
    )

-- | Change model logic
type BenignState s = StateT s (Benign IO)

-- | Doens't need to know about the model @s@
type MonadReactor c m = (AsReactor c, Alternative m, Also () m, MonadCommand c m, AskLogLevel m) -- AskPlanWeakRef, AskReactId m)

-- | Needs to know about the model @s@ and also modifies markup
-- type MonadWidget c s m = (MonadGadget c s m, AskModelWeakRef s m) --, PutReactId m, PutMarkup m)

-- | NB. 'ReactorCmd' is not a functor because of the @Widget c@ in 'MkObj'
data ReactorCmd c where
    -- -- | Evaluate some benign IO
    -- EvalBenignIO :: Benign IO c -> ReactorCmd c
    -- | Make a unique named id
    MkReactId :: NE.NonEmpty J.JSString -> (ReactId -> c) -> ReactorCmd c

    -- MkEventHandler :: NFData a
    --     => (JE.JSRep -> MaybeT IO a)
    --     -> (a -> c)
    --     -> ((J.JSVal -> IO (), IO ()) -> c)
    --     -> ReactorCmd c

    -- | Create and register a react callback
    MkEventHandler :: NFData a
        => WeakRef Plan
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> c)
        -- (preprocess, postprocess) callbacks
        -> ((J.Callback (J.JSVal -> IO ()), J.Callback (IO ())) -> c)
        -> ReactorCmd c

    -- MakeEventHandler2_ ::
    --     c
    --     -> ((J.JSVal -> IO (), IO ()) -> c)
    --     -> ReactorCmd c

    -- | Make a fully initialized object from a widget and meta
    -- MkObj :: Widget c s s () -> J.JSString -> s -> (Obj s -> c) -> ReactorCmd c
    MkWidgetRef :: Widget c s () -> NE.NonEmpty J.JSString -> s -> ((Ref Plan, Ref s) -> c) -> ReactorCmd c

    -- | Set the the prerendered backbuffer
    -- SetRender :: WeakObj s -> Window s () -> ReactorCmd c
    SetPrerendered :: WeakRef Plan -> DL.DList ReactMarkup -> ReactorCmd c

    -- Get the event target
    -- If a "ref" callback to update 'elementalRef' has not been added;
    -- then add it, rerender, then return the EventTarget.
    -- GetReactRef ::
    --     WeakModelRef s
    --     -> ReactId
    --     -> (EventTarget -> c)
    --     -> ReactorCmd c

    -- | Schedules a 'RerenderNow' to after all the current commands are processed.
    -- Does nothing if already scheduled, or in the middle of a mutation
    -- (ie 'mutations' is non empty)
    -- FIXME: Optimise also to use ReactDOM.unstable_batchedUpdates
    ScheduleRerender :: WeakRef Plan -> ReactorCmd c

    -- | Private effect used by executor:
    -- Rerender now
    -- Does nothing if rerender has not been scheduled or
    -- if in the middle of mutation ('mutations' is not empty)
    RerenderNow :: WeakRef Plan -> ReactorCmd c

    -- FIXME: TODO
    -- -- | Update, store 'ReactId' in 'mutataions' and fire 'mutatedListener'.
    -- -- The 'mutatedListener' may cause another 'Mutate' which will fire another
    -- -- 'mutatedListener' as long as the 'ReactId' has not been seen before.
    -- -- When the cycle of 'Mutate' and 'mutatedListener' finishes, the 'mutations' is
    -- -- cleared.
    -- -- Finally 'Rerender' is called.
    -- Mutate :: WeakModelRef s -> ReactId -> BenignState s c -> ReactorCmd c

    -- FIXME: TODO
    -- -- | Private effect used by executor: Calls the meta mutatedListener with 'ReactId'
    -- -- that caused the mutation.
    -- -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    -- NotifyMutated :: WeakModelRef s -> ReactId -> ReactorCmd c

    -- FIXME: TODO
    -- -- | Private effect used by executor: Resets the mutated state for this ReactId
    -- -- If there are no more pending mutations, then rerender.
    -- -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    -- ResetMutation :: WeakModelRef s -> ReactId -> ReactorCmd c

    -- FIXME: TODO
    -- -- | Create and register a dom callback
    -- RegisterDOMListener :: NFData a
    --     => WeakModelRef s
    --     -> JE.JSRep
    --     -> J.JSString
    --     -> (JE.JSRep -> MaybeT IO a)
    --     -> (a -> c)
    --     -> ReactorCmd c

    -- -- | Create and register a react callback
    -- -- If the callback is for "ref", then an listener to update 'elementalRef' for 'GetEventTarget'
    -- -- will automatically be added just before the listener in 'RegisterReactListener'.
    -- RegisterReactListener :: NFData a
    --     => WeakRef Plan
    --     -> ReactId
    --     -> J.JSString
    --     -> (JE.JSRep -> MaybeT IO a)
    --     -> (a -> c)
    --     -> ReactorCmd c

    -- -- | Create and register a callback for the mounted event
    -- RegisterMountedListener ::
    --     WeakModelRef s
    --     -> c
    --     -> ReactorCmd c

    -- -- | Create and register a callback for the rendered event
    -- RegisterRenderedListener ::
    --     WeakModelRef s
    --     -> c
    --     -> ReactorCmd c

    -- -- | Create and register a callback for the rendered event
    -- RegisterRenderedOnceListener ::
    --     WeakModelRef s
    --     -> c
    --     -> ReactorCmd c

    -- -- | Create and register a callback for the state updated event
    -- RegisterMutatedListener ::
    --     WeakModelRef s
    --     -> (ReactId -> c)
    --     -> ReactorCmd c

-- FIXME: can show ReactId and caption
instance Show (ReactorCmd c) where
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec _ (MkEventHandler _ _ _ _) = showString "MkEventHandler"
    showsPrec p (MkWidgetRef _ logname _ _) = showParen (p >= 11) $
        showString $ "MkModelRef " <> (show logname)
    -- showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec _ (SetPrerendered _ _ ) = showString "SetPrerendered"
    -- showsPrec _ (GetReactRef _ _ _) = showString "GetReactRef"
    showsPrec _ (ScheduleRerender _) = showString "ScheduleRerender"
    showsPrec _ (RerenderNow _) = showString "RerenderNow"
    -- showsPrec p (Mutate _ k _) = showParen (p >= 11) $
    --     showString "Mutate " . shows k
    -- showsPrec p (NotifyMutated _ k) = showParen (p >= 11) $
    --     showString "NotifyMutated " . shows k
    -- showsPrec p (ResetMutation _ k) = showParen (p >= 11) $
    --     showString "ResetMutation " . shows k
    -- showsPrec p (RegisterDOMListener _ k n _ _) = showParen (p >= 11) $
    --     showString "RegisterDOMListener " . shows k . showString " " . shows n
    -- showsPrec p (RegisterReactListener _ k n _ _) = showParen (p >= 11) $
    --     showString "RegisterReactListener " . shows k . showString " " . shows n
    -- showsPrec _ (RegisterMountedListener _ _) = showString "RegisterMountedListener"
    -- showsPrec _ (RegisterRenderedListener _ _) = showString "RegisterRenderedListener"
    -- -- showsPrec _ (RegisterRenderedOnceListener _ _) = showString "RegisterRenderedOnceListener"
    -- showsPrec _ (RegisterMutatedListener _ _) = showString "RegisterMutatedListener"


-- FIXME: Forgot onMounted, onUnMounted

------------------------------------------------------
-- Basic
------------------------------------------------------

-- | Make a unique named id
mkReactId :: (HasCallStack, MonadReactor c m)
    => NE.NonEmpty J.JSString -> m ReactId
mkReactId n = delegatify $ \f ->
    logExec' TRACE callStack $ MkReactId n f


-- mkEventHandler2_ ::
--     (HasCallStack, AsReactor c, MonadCommand c m, AskLogLevel m)
--     => m ()
--     -> m (J.JSVal -> IO (), IO ()) -- (preprocess, postprocess)
-- mkEventHandler2_ f = do
--     f' <- codify' f
--     delegatify $ \k -> do
--         logExec' TRACE callStack $ MakeEventHandler2 (const $ pure ()) (const f') k

-- | Make an initialized 'Obj' for a given meta using the given 'Widget'.
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkWidgetRef :: (HasCallStack, MonadReactor c m)
    => Widget c s () -> NE.NonEmpty J.JSString -> s -> m (Ref Plan, Ref s)
mkWidgetRef wid logname s = delegatify $ \f -> do
    logExec' TRACE callStack $ MkWidgetRef wid logname s f

------------------------------------------------------
-- MonadUnliftWidget
------------------------------------------------------

-- | Convenient variation of 'mkWidgetRef' where the widget is unlifted from the given monad.
-- This is useful for transformer stacks that require addition MonadReader-like effects.
unliftMkWidgetRef :: (HasCallStack, MonadUnliftWidget c s m, MonadReactor c m)
    => m () -> NE.NonEmpty J.JSString -> s -> m (Ref Plan, Ref s)
unliftMkWidgetRef m logname s = do
    u <- askUnliftWidget
    mkWidgetRef (unliftWidget u m) logname s

---------------------------
-- MonadGadget' c s m
---------------------------

-- -- | Private function: should only be called by Exec
-- setRender :: (HasCallStack, AsReactor c, MonadGadget' c s m)
--     => Window s () -> m ()
-- setRender win = do
--     obj <- askWeakModelRef
--     logExec' TRACE callStack $ SetRender obj win


-- | Rerender the ShimComponent using the current @Entity@ context
rerender :: (HasCallStack, MonadReactor c m, AskPlanWeakRef m) => m ()
rerender = do
    obj <- askPlanWeakRef
    logExec' TRACE callStack $ ScheduleRerender obj

------------------------------------------------------
-- MonadGadget
------------------------------------------------------

-- -- | Get the event target
-- -- If a "ref" callback to update 'reactRef' has not been added;
-- -- then add it, rerender, then return the EventTarget.
-- getReactRef :: (HasCallStack, AsReactor c, MonadGadget c s m) => m EventTarget
-- getReactRef = do
--     obj <- askWeakModelRef
--     i <- askReactId
--     delegatify $ \f -> logExec' TRACE callStack $ GetReactRef obj i f

-- -- | mutates the meta for the weakObj.
-- mutate :: (HasCallStack, MonadGadget c s m) => BenignState s () -> m ()
-- mutate m = do
--     obj <- askWeakModelRef
--     i <- askReactId
--     logExec' TRACE callStack $ Mutate obj i (command_ <$> m)
-- --     logExec' TRACE callStack $ Mutate obj k (command_ <$> (zoom sbj m))

-- -- | Update the 'Meta' using the current @Entity@ context,
-- -- and also return the next action to execute.
-- mutateThen :: (HasCallStack, MonadGadget c s m) => BenignState s (m a) -> m a
-- mutateThen m = do
--     obj <- askWeakModelRef
--     i <- askReactId
--     delegate $ \fire -> do
--         -- f :: m a -> m ()
--         let f n = n >>= fire
--         -- f' :: m a -> c
--         f' <- codify f
--         logExec' TRACE callStack $ Mutate obj i (f' <$> m)
--         -- logExec' TRACE callStack $ Mutate obj k (f' <$> (zoomUnder (iso Als getAls) sbj m))

-- -- | Same as 'onRendered' but the action only occurs once on the next rerender.
-- onRenderedOnce ::
--     (HasCallStack, AsReactor c, MonadGadget' c s m)
--     => m a -> m a
-- onRenderedOnce m = do
--     obj <- askWeakModelRef
--     delegate $ \fire -> do
--         f <- codify' (m >>= fire)
--         logExec' TRACE callStack $ RegisterRenderedOnceListener obj f

------------------------------------------------------
-- Triggers - MonadGadget')
------------------------------------------------------

-- -- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- -- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- -- Contrast with 'trigger' which expects a 'Notice'.
-- domTrigger ::
--     (HasCallStack, NFData a, AsReactor c, MonadGadget' c s m)
--     => JE.JSRep
--     -> J.JSString
--     -> (JE.JSRep -> MaybeT IO a)
--     -> m a
-- domTrigger j n goStrict = do
--     obj <- askWeakModelRef
--     delegatify $ \f ->
--         logExec' TRACE callStack $ RegisterDOMListener obj j n goStrict f

-- -- | A variation of trigger which ignores the event but fires the given arg instead.
-- -- Unlike 'domTrigger' the @a@ does not need to be @NFData a@
-- domTrigger_ ::
--     (HasCallStack, AsReactor c, MonadGadget' c s m)
--     => JE.JSRep
--     -> J.JSString
--     -> a
--     -> m a
-- domTrigger_ j n a = do
--     runGadget $ domTrigger j n (const $ pure ())
--     pure a

-- -- | Register actions to execute after a render.
-- -- It is safe to 'exec'' a 'Mutate' or 'Rerender' in this callback.
-- -- These command will not trigger another rendered event.
-- --
-- -- NB. This is trigged by react 'componentDidMount'
-- -- See jsbits/react.js hgr$shimComponent.
-- -- These callbacks are called after the ref callback by React
-- -- See https://reactjs.org/docs/refs-and-the-dom.html.
-- onMounted ::
--     (HasCallStack, AsReactor c, MonadGadget' c s m)
--     => m a
--     -> m a
-- onMounted m = do
--     obj <- askWeakModelRef
--     delegate $ \fire -> do
--         c <- codify' (m >>= fire)
--         logExec' TRACE callStack $ RegisterMountedListener obj c

-- -- | Register actions to execute after a render.
-- -- It is safe to 'exec'' a 'Mutate' or 'Rerender'. These command will not
-- -- trigger another rendered event.
-- --
-- -- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- -- so it is also called for the initial render.
-- -- See jsbits/react.js hgr$shimComponent.
-- -- These callbacks are called after the ref callback by React
-- -- See https://reactjs.org/docs/refs-and-the-dom.html.
-- onRendered ::
--     (HasCallStack, AsReactor c, MonadGadget' c s m)
--     => m a
--     -> m a
-- onRendered m = do
--     obj <- askWeakModelRef
--     delegate $ \fire -> do
--         f <- codify' (m >>= fire)
--         logExec' TRACE callStack $ RegisterRenderedListener obj f

-- -- | Register actions to execute after the state has been updated with 'mutate'.
-- -- The callback is called` with the 'ReactId' that was passed into 'mutate'.
-- -- To prevent infinite loops, if this 'onMutated' causes another 'mutate'
-- -- with the same 'ReactId', then another 'onMutated' callback will NOT be fired.
-- onMutated ::
--     (HasCallStack, AsReactor c, MonadGadget' c s m)
--     => (ReactId -> m a) -> m a
-- onMutated f = do
--     obj <- askWeakModelRef
--     delegate $ \fire -> do
--         g <- codify ((fire =<<) . f)
--         logExec' TRACE callStack $ RegisterMutatedListener obj g

-- -- -- | Variation of 'onMutated' where you don't care about the 'ReactId' that caused the rerender.
-- -- onMutated' ::
-- --     (HasCallStack, MonadGadget' c s m)
-- --     => obj -> m a -> m a
-- -- onMutated' f = onMutated (const f)

---------------------------
-- Triggers - MonadGadget
---------------------------

-- | This convert the input @goStrict@ and @f@ into two ghcjs 'Callback',
-- a (preprocess callback, postprocess callback).
-- Multiple preprocess callback must be run for the same event before any of the
-- postprocess callback, due to the way ghcjs sync and async threads interact with React js.
-- See comment for mkEventProcessor in Glazier.React.Reactor.Exec
mkEventHandler ::
    (HasCallStack, NFData a, MonadReactor c m, AskPlanWeakRef m)
    => (JE.JSRep -> MaybeT IO a)
    -> (a -> m ())
    -> m (J.Callback (J.JSVal -> IO ()), J.Callback (IO ())) -- (preprocess, postprocess)
mkEventHandler goStrict f = do
    plnRef <- askPlanWeakRef
    f' <- codify f
    delegatify $ \k -> do
        logExec' TRACE callStack $ MkEventHandler plnRef goStrict f' k

-- -- | Create a callback for a 'JE.JSRep' and add it to this reactant's dlist of listeners.
-- -- Consider using the other callbacks because all react listeners result in 'Notice'.
-- trigger' ::
--     (HasCallStack, NFData a, MonadReactor c m, AskPlanWeakRef m, AskReactId m)
--     => J.JSString
--     -> (JE.JSRep -> MaybeT IO a)
--     -> (a -> m b)
--     -> m b
-- trigger' n goStrict f = do
--     obj <- askPlanWeakRef
--     i <- askReactId
--     a <- delegatify $ \k -> do
--         logExec' TRACE callStack $ RegisterReactListener obj i n goStrict k
--     f a

-- -- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
-- -- Contrast with 'domTrigger' which expects a 'JE.JSRep'.
-- trigger ::
--     (HasCallStack, NFData a, AsReactor c, MonadReactor c m)
--     => J.JSString
--     -> (Notice -> MaybeT IO a)
--     -> (a -> m b)
--     -> m b
-- trigger n goStrict k = trigger' n (handlesNotice goStrict) k
--   where
--     handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
--     handlesNotice g j = MaybeT (pure $ JE.fromJSRep j) >>= g

-- -- | A variation of 'trigger' which ignores the event but fires the given arg instead.
-- -- Unlike 'trigger' the @a@ does not need to be @NFData a@
-- trigger_ ::
--     (HasCallStack, AsReactor c, MonadReactor c m)
--     => J.JSString
--     -> a
--     -> (a -> m b)
--     -> m b  
-- trigger_ n a k =
--     -- using trigger' to bypass conversion to 'Notice' in 'trigger'
--     runGadget $ trigger' n (const $ pure ()) (const $ k a)

-- trigger2 ::
--     (HasCallStack, NFData a, AsReactor c, MonadGadget c s m)
--     => J.JSString
--     -> (Notice -> MaybeT IO a)
--     -> (a -> m ())

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
