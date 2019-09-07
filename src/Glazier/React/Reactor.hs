-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor where

import Control.Also
import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Context
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Function.Extras
import Data.IORef
import qualified Data.JSString as J
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import GHC.Stack
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.EventTarget
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Notice
import Glazier.React.ReactId
import Glazier.React.Widget
import Glazier.ShowIO
import System.Mem.Weak
-- import Glazier.React.Widget.Internal
import qualified JavaScript.Extras as JE

-----------------------------------------------------------------

type CmdReactor c =
    ( Cmd' [] c -- required by 'command_'
    , Cmd' Reactor c
    , Cmd LogLine c
    )

-- | Change model logic
-- type BenignState s = StateT s IO

-- | Doesn't need to know about the model @s@
type MonadReactor c m = (Cmd' [] c, CmdReactor c, Alternative m, Also () m, MonadCommand c m, AskLogLevel m) -- AskPlanWeakRef, AskReactId m)

-- | Needs to know about the model @s@ and also modifies markup
-- type MonadWidget c s m = (MonadGadget c s m, AskModelWeakRef s m) --, PutReactId m, PutMarkup m)

-- | NB. 'Reactor' is not a functor because of the @Widget c@ in 'MkObj'
data Reactor c where
    -- -- -- | Evaluate some benign IO
    -- -- EvalBenignIO :: Benign IO c -> Reactor c
    -- -- | Make a unique named id
    -- MkReactId :: NE.NonEmpty J.JSString -> (ReactId -> c) -> Reactor c

    -- MkEventHandler :: NFData a
    --     => (JE.JSRep -> MaybeT IO a)
    --     -> (a -> c)
    --     -> ((J.JSVal -> IO (), IO ()) -> c)
    --     -> Reactor c

    -- | Using the NFData idea from React/Flux/PropertiesAndEvents.hs
    -- React re-uses Notice from a pool, which means it may no longer be valid if we lazily
    -- parse it. However, we still want lazy parsing so we don't parse unnecessary fields.
    -- Additionally, we don't want to block during the event handling.The reason this is a problem is
    -- because Javascript is single threaded, but Haskell is lazy.
    -- Therefore GHCJS threads are a strange mixture of synchronous and asynchronous threads,
    -- where a synchronous thread might be converted to an asynchronous thread if a "black hole" is encountered.
    -- See https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Concurrent.hs
    -- This safe interface requires two input functions:
    -- 1. a function to reduce Notice to a NFData. The handleEvent will ensure that the
    -- NFData is forced which will ensure all the required fields from Synthetic event has been parsed.
    -- This function must not block.
    -- 2. a second function that uses the NFData. This function is allowed to block.
    -- handleEvent results in a function that you can safely pass into 'GHC.Foreign.Callback.syncCallback1'
    -- with 'GHCJS.Foreign.Callback.ContinueAsync'.
    -- I have innovated further with the NFData idea to return two functions:
    -- 1. (evt -> IO ()) function to preprocess the event, which is guaranteed to be non blocking.
    -- 2. An IO () postprocessor function which may block.
    -- This allows for multiple handlers for the same event to be processed safely,
    -- by allowing a way for all the preprocessor handlers to run first before
    -- running all of the postprocessor handlers.
    --
    -- GHC will try to return the same processor for the same input functions as much as possible
    -- so that it will be relatively efficient to use this function on every rerender.
    MkHandler :: NFData a
        => WeakRef Plan
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> c)
        -- (preprocess, postprocess)
        -> ((JE.JSRep -> IO (), IO ()) -> c)
        -> Reactor c

    -- | Turn processor into a 'J.Callback'
    -- GHC will try to return the same callback for the same input functions as much as possible.
    -- so that it will be relatively efficient to use this function on every rerender.
    MkCallback ::
        WeakRef Plan
        -- (preprocess, postprocess)
        -> (JE.JSRep -> IO (), IO ())
        -> (J.Callback (J.JSVal -> IO ()) -> c)
        -> Reactor c

    -- | Make a fully initialized object from a widget and meta
    -- MkObj :: Widget c s s () -> J.JSString -> s -> (Obj s -> c) -> Reactor c
    MkObj :: Widget c s () -> NE.NonEmpty J.JSString -> s -> (Obj s -> c) -> Reactor c

    -- | Set the the prerendered backbuffer
    -- SetRender :: WeakObj s -> Window s () -> Reactor c
    SetPrerendered :: WeakRef Plan -> DL.DList ReactMarkup -> Reactor c

    -- Get the event target
    -- If a "ref" callback to update 'elementalRef' has not been added;
    -- then add it, rerender, then return the EventTarget.
    -- GetReactRef ::
    --     WeakModelRef s
    --     -> ReactId
    --     -> (EventTarget -> c)
    --     -> Reactor c

    -- | Schedules a 'RerenderNow' to after all the current commands are processed.
    -- Does nothing if already scheduled, or in the middle of a mutation
    -- (ie 'mutations' is non empty)
    -- FIXME: Optimise also to use ReactDOM.unstable_batchedUpdates
    ScheduleRerender :: WeakRef Plan -> Reactor c

    -- | Private effect used by executor:
    -- Rerender now
    -- Does nothing if rerender has not been scheduled or
    -- if in the middle of mutation ('mutations' is not empty)
    RerenderNow :: WeakRef Plan -> Reactor c

    -- FIXME: TODO
    -- -- | Update, store 'ReactId' in 'mutataions' and fire 'mutatedListener'.
    -- -- The 'mutatedListener' may cause another 'Mutate' which will fire another
    -- -- 'mutatedListener' as long as the 'ReactId' has not been seen before.
    -- -- When the cycle of 'Mutate' and 'mutatedListener' finishes, the 'mutations' is
    -- -- cleared.
    -- -- Finally 'Rerender' is called.
    -- Mutate :: WeakModelRef s -> ReactId -> StateT s IO c -> Reactor c
    Mutate :: WeakRef s -> Rerender -> StateT s IO c -> Reactor c

    -- FIXME:: Get another widget's model in a safe way where this will be registered
    -- as a listener to be notified of mutations
    GetObjModel :: WeakRef Plan -> WeakObj s -> (s -> c) -> Reactor c

    -- FIXME: TODO
    -- -- | Private effect used by executor: Calls the meta mutatedListener with 'ReactId'
    -- -- that caused the mutation.
    -- -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    -- NotifyMutated :: WeakModelRef s -> ReactId -> Reactor c

    -- FIXME: TODO
    -- -- | Private effect used by executor: Resets the mutated state for this ReactId
    -- -- If there are no more pending mutations, then rerender.
    -- -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    -- ResetMutation :: WeakModelRef s -> ReactId -> Reactor c

    -- FIXME: TODO
    -- -- | Create and register a dom callback
    -- RegisterDOMListener :: NFData a
    --     => WeakModelRef s
    --     -> JE.JSRep
    --     -> J.JSString
    --     -> (JE.JSRep -> MaybeT IO a)
    --     -> (a -> c)
    --     -> Reactor c

    -- -- | Create and register a react callback
    -- -- If the callback is for "ref", then an listener to update 'elementalRef' for 'GetEventTarget'
    -- -- will automatically be added just before the listener in 'RegisterReactListener'.
    -- RegisterReactListener :: NFData a
    --     => WeakRef Plan
    --     -> ReactId
    --     -> J.JSString
    --     -> (JE.JSRep -> MaybeT IO a)
    --     -> (a -> c)
    --     -> Reactor c

    -- -- | Create and register a callback for the mounted event
    -- RegisterMountedListener ::
    --     WeakModelRef s
    --     -> c
    --     -> Reactor c

    -- -- | Create and register a callback for the rendered event
    -- RegisterRenderedListener ::
    --     WeakModelRef s
    --     -> c
    --     -> Reactor c

    -- -- | Create and register a callback for the rendered event
    -- RegisterRenderedOnceListener ::
    --     WeakModelRef s
    --     -> c
    --     -> Reactor c

    -- -- | Create and register a callback for the state updated event
    -- RegisterMutatedListener ::
    --     WeakModelRef s
    --     -> (ReactId -> c)
    --     -> Reactor c

instance ShowIO (Reactor c) where
    -- showsPrecIO p (MkReactId s _) = textParen (p >= 11) $
    --     "MkReactId " <> showIO s
    showsPrecIO p (MkHandler this _ _ _) = showParenIO (p >= 11) $ ("MkHandler " <>) <$> (showIO this)
    showsPrecIO p (MkObj _ logname _ _) = showParenIO (p >= 11) $ ("MkObj " <>) <$> (showIO logname)
    -- showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrecIO p (SetPrerendered this _ ) = showParenIO (p >= 11) $ ("SetPrerendered " <>) <$> (showIO this)
    -- showsPrec _ (GetReactRef _ _ _) = showString "GetReactRef"
    showsPrecIO p (ScheduleRerender this) = showParenIO (p >= 11) $ ("ScheduleRerender " <>) <$> (showIO this)
    showsPrecIO p (RerenderNow this) = showParenIO (p >= 11) $ ("RerenderNow " <>) <$> (showIO this)
    showsPrecIO p (Mutate _ r _) = showParenIO (p >= 11) $ pure $ "Mutate " <> (T.pack $ show r)
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

-- -- | Make a unique named id
-- mkReactId :: (HasCallStack, MonadReactor c m)
--     => NE.NonEmpty J.JSString -> m ReactId
-- mkReactId n = delegatify $ \f ->
--     logExec' TRACE callStack $ MkReactId n f


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
mkObj :: (HasCallStack, MonadReactor c m)
    => Widget c s () -> NE.NonEmpty J.JSString -> s -> m (Obj s)
mkObj wid logname s = delegatify $ \f -> do
    logExec' TRACE callStack $ MkObj wid logname s f

------------------------------------------------------
-- MonadUnliftWidget
------------------------------------------------------

-- | Convenient variation of 'mkObj' where the widget is unlifted from the given monad.
-- This is useful for transformer stacks that require addition MonadReader-like effects.
unliftMkObj :: (HasCallStack, MonadUnliftWidget c s m, MonadReactor c m)
    => m () -> NE.NonEmpty J.JSString -> s -> m (Obj s)
unliftMkObj m logname s = do
    u <- askUnliftWidget
    mkObj (unliftWidget u m) logname s

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
    this <- askPlanWeakRef
    logExec' TRACE callStack $ ScheduleRerender this

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

-- | 'mutate_' with 'RerenderRequired'
mutate :: (HasCallStack, MonadReactor c m, AskModelWeakRef s m) => StateT s IO () -> m ()
mutate = mutate_ RerenderRequired

-- | Mutates the Model for the current widget.
-- Expose 'Rerender' for "uncontrolled (by react)" components like <input>
-- which doesn't necessarily require a rerender if the model changes.
-- Mutation notifications are always sent.
mutate_ :: (HasCallStack, MonadReactor c m, AskModelWeakRef s m) => Rerender -> StateT s IO () -> m ()
mutate_ r m = do
    this <- askModelWeakRef
    logExec' TRACE callStack $ Mutate this r (command_ <$> m)

-- | 'mutateThen_' with 'RerenderRequired'
mutateThen :: (HasCallStack, MonadReactor c m, AskModelWeakRef s m) => StateT s IO (m a) -> m a
mutateThen = mutateThen_ RerenderRequired

-- | Variation of 'mutate_' which also returns the next action to execute after mutating.
mutateThen_ :: (HasCallStack, MonadReactor c m, AskModelWeakRef s m) => Rerender -> StateT s IO (m a) -> m a
mutateThen_ r m = do
    this <- askModelWeakRef
    delegate $ \fire -> do
        -- f :: m a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> c
        f' <- codify f
        logExec' TRACE callStack $ Mutate this r (f' <$> m)

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
-- a (callback, postprocess).
-- Multiple preprocess callback must be run for the same event before any of the
-- postprocess callback, due to the way ghcjs sync and async threads interact with React js.
mkHandler ::
    (HasCallStack, NFData a, MonadReactor c m, AskPlanWeakRef m)
    => (JE.JSRep -> MaybeT IO a)
    -> (a -> m ())
    -> m (JE.JSRep -> IO (), IO ()) -- (preprocess, postprocess)
mkHandler goStrict f = do
    plnRef <- askPlanWeakRef
    f' <- codify f
    delegatify $ \k -> do
        logExec' TRACE callStack $ MkHandler plnRef goStrict f' k

handleNotice :: Monad m => (Notice -> MaybeT m a) -> (JE.JSRep -> MaybeT m a)
handleNotice g j = MaybeT (pure $ JE.fromJSRep j) >>= g

mkNoticeHandler ::
    (HasCallStack, NFData a, MonadReactor c m, AskPlanWeakRef m)
    => (Notice -> MaybeT IO a)
    -> (a -> m ())
    -> m (JE.JSRep -> IO (), IO ()) -- (preprocess, postprocess)
mkNoticeHandler goStrict goLazy = mkHandler (handleNotice goStrict) goLazy

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
