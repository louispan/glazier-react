{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Glazier.React.Reactor
    ( AsReactor
    , ModelState
    , MonadReactor
    , ReactorCmd(..)
    , LogLevel(..)
    , logTrace
    , logDebug
    , logInfo
    , logWarn
    , logError
    , tracedExec
    , tracedExec'
    , tracedEval
    , tracedEval'
    , debugIO
    , debugIO_
    , debugIOThen
    , mkReactId
    , setRender
    , mkObj
    , mkObj'
    , withMkObj
    , getModel
    , getElementalRef
    , rerender
    , mutate
    , mutateThen
    , domTrigger
    , domTrigger_
    , trigger
    , trigger_
    , onMounted
    , onRendered
    , onNextRendered
    , onMutated
    , onMutated'
    ) where

import Control.Also
import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString
import Data.Diverse.Lens
import qualified Data.JSString as J
import GHC.Stack
import Glazier.Benign
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.EventTarget
import Glazier.React.Notice
import Glazier.React.Obj
import Glazier.React.ReactId
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE

-----------------------------------------------------------------

-- | Without 'HasCallStack'
type AsReactor c =
    ( AsFacet [c] c -- implicity required by 'MonadCodify'
    , AsFacet (ReactorCmd c) c
    )

type ModelState s = StateT s (Benign IO)

type MonadReactor c o s m =
    ( AsReactor c
    , MonadReader (Entity o s) m
    , MonadCommand c m
    )

data LogLevel
    -- Reactor commands are automatically logged at trace level
    = LogTrace
    -- console.log (which is actually an alias of console.info)
    | LogDebug
    -- console.info
    | LogInfo
    -- console.warn
    | LogWarn
    -- console.error, will also print callstack
    | LogError
    deriving (Eq, Show, Read, Ord)


-- | NB. 'ReactorCmd' is not a functor because of the @Widget c@ in 'MkObj'
data ReactorCmd c where
#ifdef DEBUGIO
    -- | run arbitrary IO, should only be used for debugging
    DebugIO :: IO c -> ReactorCmd c
#endif
    -- Log something to console
    LogLn :: CallStack -> LogLevel -> Benign IO J.JSString -> ReactorCmd c
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> c) -> ReactorCmd c
    -- | the the rendering function in a Obj, replace any existing render callback
    SetRender :: WeakObj s -> Window s () -> ReactorCmd c
    -- | Make a fully initialized object from a widget and model
    MkObj :: Widget c s s () -> s -> (Obj s -> c) -> ReactorCmd c
    -- | Get the model
    GetModel :: WeakObj s -> (s -> c) -> ReactorCmd c
    -- Get the event target
    -- If a "ref" callback to update 'elementalRef' has not been added;
    -- then add it, rerender, then return the EventTarget.
    GetElementalRef ::
        WeakObj s
        -> ReactId
        -> (EventTarget -> c)
        -> ReactorCmd c
    -- | Rerender a ShimComponent using the given state.
    Rerender :: WeakObj s -> ReactorCmd c
    -- | Private: Renders the object (will only do something first time after Rerender)
    DoRerender :: WeakObj s -> ReactorCmd c
    -- | Update and rerender.
    Mutate :: WeakObj s -> ReactId -> ModelState s c -> ReactorCmd c
    -- | Private: Calls the model mutatedListener
    -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    NotifyMutated :: WeakObj s -> ReactId -> ReactorCmd c
    -- | Private: Resets the mutated state for this ReactId
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
    RegisterNextRenderedListener ::
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
#ifdef DEBUGIO
    showsPrec _ (DebugIO _ ) = showString "DebugIO"
#endif
    showsPrec p (LogLn stk lvl _) = showParen (p >= 11) $
        showString "LogLn " . shows lvl . showString " at " . showString (prettyCallStack stk)
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec p (MkObj _ _ _) = showParen (p >= 11) $
        showString "MkObj "
    showsPrec _ (GetModel _ _) = showString "GetModel"
    showsPrec _ (GetElementalRef _ _ _) = showString "GetElementalRef"
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (DoRerender _) = showString "DoRreender_"
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
    showsPrec _ (RegisterNextRenderedListener _ _) = showString "RegisterNextRenderedListener"
    showsPrec _ (RegisterMutatedListener _ _) = showString "RegisterMutatedListener"


------------------------------------------------------

-- | Variation of 'debugIO_' where the IO actino returns
-- the next command to process
-- If DEBUGIO is not defined, then this does nothing.
debugIO :: (HasCallStack, AsReactor c, MonadCommand c m) => IO c -> m ()
#ifdef DEBUGIO
debugIO m = tracedExec' callStack $ DebugIO m
#else
debugIO _ = pure ()
#endif

-- | Run an arbitrary IO. This should only be used for testing.
-- If DEBUGIO is not defined, then this does nothing.
debugIO_ :: (HasCallStack, AsReactor c, MonadCommand c m) => IO () -> m ()
#ifdef DEBUGIO
debugIO_ m = tracedExec' callStack $ DebugIO (command_ <$> m)
#else
debugIO_ _ = pure ()
#endif

-- | Variation of 'debugIO' where the IO action returns
-- the next comamand to process.
-- If DEBUGIO is not defined, then this does nothing.
debugIOThen :: (HasCallStack, AsReactor c, MonadCommand c m) => IO (m a) -> m a
#ifdef DEBUGIO
debugIOThen m =
    delegate $ \fire -> do
        -- f :: n a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> c
        f' <- codify f
        tracedExec' callStack $ DebugIO (f' <$> m)
#else
debugIOThen _ = finish (pure ())
#endif

{-# WARNING debugIO, debugIO_, debugIOThen "Use this for debugging only. It will be disabled when DEBUGIO, ie cabal flag(debug), is not set" #-}

-- logLn :: (AsReactor c, MonadCommand c m, HasCallStack) => LogLevel -> Benign IO J.JSString -> m ()
-- logLn lvl m = exec' $ LogLn callStack lvl m

-- | @console.info("TRACE " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- NB. Reactor commands are automatically logged at trace level.
logTrace :: (HasCallStack, AsReactor c, MonadCommand c m) => Benign IO J.JSString -> m ()
logTrace m = exec' $ LogLn callStack LogTrace m

-- | @console.info("DEBUG " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
logDebug :: (HasCallStack, AsReactor c, MonadCommand c m) => Benign IO J.JSString -> m ()
logDebug m = exec' $ LogLn callStack LogDebug m

-- | @console.info("INFO  " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
logInfo :: (HasCallStack, AsReactor c, MonadCommand c m) => Benign IO J.JSString -> m ()
logInfo m = exec' $ LogLn callStack LogInfo m

-- | @console.warn("WARN  " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
logWarn :: (HasCallStack, AsReactor c, MonadCommand c m) => Benign IO J.JSString -> m ()
logWarn m = exec' $ LogLn callStack LogWarn m

-- | @console.error("ERROR " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- Will print the full callstack, not just the first level.
logError :: (HasCallStack, AsReactor c, MonadCommand c m) => Benign IO J.JSString -> m ()
logError m = exec' $ LogLn callStack LogError m

showCmd :: Show c => c -> Benign IO J.JSString
showCmd c = pure (J.pack $ show c)

tracedExec :: (Show cmd, AsReactor c, AsFacet cmd c, MonadCommand c m)
    => CallStack -> cmd -> m ()
tracedExec stk c = do
    exec' $ LogLn stk Trace (showCmd c)
    exec c

tracedExec' :: (Show (cmd c), AsReactor c, AsFacet (cmd c) c, MonadCommand c m)
    => CallStack -> cmd c -> m ()
tracedExec' stk c = do
    exec' $ LogLn stk Trace (showCmd c)
    exec' c

tracedEval' :: (Show (cmd c), AsReactor c, AsFacet [c] c, AsFacet (cmd c) c, MonadCommand c m)
    => CallStack -> ((a -> c) -> cmd c) -> m a
tracedEval' stk k = eval_ $ tracedExec' stk . k

tracedEval :: (Show cmd, AsReactor c, AsFacet [c] c, AsFacet cmd c, MonadCommand c m)
    => CallStack -> ((a -> c) -> cmd) -> m a
tracedEval stk k = eval_ $ tracedExec stk . k

-- | Make a unique named id
mkReactId :: (HasCallStack, AsReactor c, MonadCommand c m)
    => J.JSString -> m ReactId
mkReactId n = delegate $ \fire -> do
    f <- codify fire
    tracedExec' callStack $ MkReactId n f

setRender :: (HasCallStack, AsReactor c, MonadCommand c m)
    => WeakObj s -> Window s () -> m ()
setRender obj win = tracedExec' callStack $ SetRender obj win

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkObj :: (HasCallStack, AsReactor c, MonadCommand c m)
    => Widget c s s a -> s -> m (Either a (Obj s))
mkObj wid s = delegate $ \fire -> do
    f <- codify fire
    let wid' = wid >>= (instruct . f . Left)
    tracedExec' callStack $ MkObj wid' s (f . Right)

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkObj' :: (HasCallStack, AsReactor c, MonadCommand c m)
    => Widget c s s () -> s -> m (Obj s)
mkObj' gad s = delegate $ \fire -> do
    f <- codify fire
    tracedExec' callStack $ MkObj gad s f

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
withMkObj :: (HasCallStack, AsReactor c, MonadCommand c m)
    => Widget c s s a -> s -> (Obj s -> m ()) -> m a
withMkObj wid s k = delegate $ \fire -> do
    f <- codify fire
    k' <- codify k
    let wid' = wid >>= (instruct . f)
    tracedExec' callStack $ MkObj wid' s k'

-- | Rerender the ShimComponent using the current @Entity@ context
rerender :: (HasCallStack, MonadReactor c o s m) => m ()
rerender = do
    obj <- view _weakObj
    tracedExec' callStack $ Rerender obj

-- | Get the 'Model' and exec actions, using the current @Entity@ context
getModel :: (HasCallStack, MonadReactor c o s m) => m s
getModel = delegate $ \fire -> do
    Entity {..} <- ask
    let fire' s = case preview this s of
            Nothing -> pure ()
            Just s' -> fire s'
    c <- codify fire'
    tracedExec' callStack $ GetModel entity c

-- | Get the event target
-- If a "ref" callback to update 'elementalRef' has not been added;
-- then add it, rerender, then return the EventTarget.
getElementalRef :: (HasCallStack, MonadReactor c o s m) => ReactId -> m EventTarget
getElementalRef k = delegate $ \fire -> do
    Entity {..} <- ask
    c <- codify fire
    tracedExec' callStack $ GetElementalRef entity k c

-- | Update the 'Model' using the current @Entity@ context
mutate :: (HasCallStack, MonadReactor c o s m) => ReactId -> ModelState s () -> m ()
mutate k m = do
    Entity {..} <- ask
    let m' = zoom this m
    tracedExec' callStack $ Mutate entity k (command_ <$> m')

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
mutateThen :: (HasCallStack, Also m a, MonadReactor c o s m) => ReactId -> ModelState s (m a) -> m a
mutateThen k m = do
    Entity {..} <- ask
    delegate $ \fire -> do
        let m' = getAls <$> zoom this (Als <$> m)
            -- f :: m a -> m ()
            f n = n >>= fire
        -- f' :: m a -> c
        f' <- codify f
        tracedExec' callStack $ Mutate entity k (f' <$> m')

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- Contrast with 'trigger' which expects a 'Notice'.
__domTrigger ::
    ( NFData a
    , MonadReactor c o s m
    )
    => CallStack
    -> JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
__domTrigger stk j n goStrict = delegate $ \goLazy -> do
    Entity {..} <- ask
    goLazy' <- codify goLazy
    tracedExec' stk $ RegisterDOMListener entity j n goStrict goLazy'

domTrigger ::
    ( HasCallStack
    , NFData a
    , MonadReactor c o s m
    )
    => JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
domTrigger = __domTrigger callStack

-- | A variation of trigger which ignores the event but fires the given arg instead.
domTrigger_ ::
    (HasCallStack, MonadReactor c o s m)
    => JE.JSRep
    -> J.JSString
    -> a
    -> m a
domTrigger_ j n a = do
    __domTrigger callStack j n (const $ pure ())
    pure a

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- Not exposed because all react listeners result in 'Notice'.
__trigger ::
    ( NFData a
    , MonadReactor c o s m
    )
    => CallStack
    -> ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
__trigger stk k n goStrict = delegate $ \goLazy -> do
    Entity {..} <- ask
    goLazy' <- codify goLazy
    tracedExec' stk $ RegisterReactListener entity k n goStrict goLazy'

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
-- Contrast with 'domTrigger' which expects a 'JE.JSRep'.
trigger ::
    ( HasCallStack
    , NFData a
    , MonadReactor c o s m
    )
    => ReactId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger k n goStrict = __trigger callStack k n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice g j = MaybeT (pure $ JE.fromJSR j) >>= g

-- | A variation of 'trigger' which ignores the event but fires the given arg instead.
trigger_ ::
    ( HasCallStack
    , MonadReactor c o s m
    )
    => ReactId
    -> J.JSString
    -> a
    -> m a
trigger_ k n a = do
    -- using '__trigger' to bypass conversion to 'Notice' in 'trigger'
    __trigger callStack k n (const $ pure ())
    pure a

-- | Register actions to execute after a render.
-- It is safe to 'exec'' a 'Mutate' or 'Rerender'. These command will not
-- trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidMount'
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onMounted ::
    (HasCallStack, MonadReactor c o s m)
    => m a
    -> m a
onMounted m = do
    Entity {..} <- ask
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        tracedExec' callStack $ RegisterMountedListener entity c

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
    (HasCallStack, MonadReactor c o s m)
    => m a
    -> m a
onRendered m = do
    obj <- view _weakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        tracedExec' callStack $ RegisterRenderedListener obj c

-- | Same as 'onRendered' but the action only occurs once on the next rerender.
onNextRendered ::
    (HasCallStack, MonadReactor c o s m)
    => m a -> m a
onNextRendered m = do
    obj <- view _weakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        tracedExec' callStack $ RegisterNextRenderedListener obj c

-- | Register actions to execute after the state has been updated with 'mutate'.
-- The callback is called` with the 'ReactId' that was passed into 'mutate'.
-- To prevent infinite loops, if this 'onMutated' causes another 'mutate'
-- with the same 'ReactId', then another 'onMutated' callback will NOT be fired.
__onMutated ::
    (MonadReactor c o s m)
    => CallStack -> (ReactId -> m a) -> m a
__onMutated stk f = do
    obj <- view _weakObj
    delegate $ \fire -> do
        g <- codify ((fire =<<) . f)
        tracedExec' stk $ RegisterMutatedListener obj g

onMutated ::
    (HasCallStack, MonadReactor c o s m)
    => (ReactId -> m a) -> m a
onMutated = __onMutated callStack

-- | Variation of 'onMutated' where you don't care about the 'ReactId' that caused the rerender.
onMutated' ::
    (HasCallStack, MonadReactor c o s m)
    => m a -> m a
onMutated' f = __onMutated callStack (const f)



wack ::
    ( HasCallStack
    , AsReactor c
    , MonadCommand c m
    , FromJSON s
    ) => ByteString
    -> Maybe (m a)
wack = undefined