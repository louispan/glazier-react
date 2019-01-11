{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor where

import Control.Also
import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Aeson.Applicative as A
import Data.Diverse.Lens
import qualified Data.JSString as J
import GHC.Stack
import Glazier.Benign
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.EventTarget
import Glazier.React.Model
import Glazier.React.Notice
import Glazier.React.Obj
import Glazier.React.ReactId
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE
import JavaScript.Extras.Aeson.Instances ()

-----------------------------------------------------------------

-- | Without 'HasCallStack'
type AsReactor c =
    ( AsFacet [c] c -- implicity required by 'MonadCodify'
    , AsFacet (ReactorCmd c) c
    )

type ModelState s = StateT s (Benign IO)

type MonadReactor c m =
    ( AsReactor c
    , MonadCommand c m
    )

type MonadGadget c o s m =
    ( MonadReader (Entity o s) m
    , MonadReactor c m
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
    -- | Evaluate some benign IO
    EvalBenignIO :: Benign IO a -> (a -> c) -> ReactorCmd c
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> c) -> ReactorCmd c
    -- | Set the the rendering function in a Obj, replace any existing render callback
    SetRender :: WeakObj s -> Window s () -> ReactorCmd c
    -- | Make a fully initialized object from a widget and model
    MkObj :: Widget c s s () -> s -> (Obj s -> c) -> ReactorCmd c
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
    showsPrec _ (EvalBenignIO _ _) = showString "EvalBenignIO"
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec p (MkObj _ _ _) = showParen (p >= 11) $
        showString "MkObj "
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
debugIO :: (HasCallStack, MonadReactor c m) => IO c -> m ()
#ifdef DEBUGIO
debugIO m = tracedExec' callStack $ DebugIO m
#else
debugIO _ = pure ()
#endif

-- | Run an arbitrary IO. This should only be used for testing.
-- If DEBUGIO is not defined, then this does nothing.
debugIO_ :: (HasCallStack, MonadReactor c m) => IO () -> m ()
#ifdef DEBUGIO
debugIO_ m = tracedExec' callStack $ DebugIO (command_ <$> m)
#else
debugIO_ _ = pure ()
#endif

-- | Variation of 'debugIO' where the IO action returns
-- the next comamand to process.
-- If DEBUGIO is not defined, then this does nothing.
debugIOThen :: (HasCallStack, MonadReactor c m) => IO (m a) -> m a
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

-- logLn :: (HasCallStack, MonadReactor c m) => LogLevel -> Benign IO J.JSString -> m ()
-- logLn lvl m = exec' $ LogLn callStack lvl m

-- | @console.info("TRACE " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- NB. Reactor commands are automatically logged at trace level.
logTrace :: (HasCallStack, MonadReactor c m) => Benign IO J.JSString -> m ()
logTrace m = exec' $ LogLn callStack LogTrace m

-- | @console.info("DEBUG " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
logDebug :: (HasCallStack, MonadReactor c m) => Benign IO J.JSString -> m ()
logDebug m = exec' $ LogLn callStack LogDebug m

-- | @console.info("INFO  " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
logInfo :: (HasCallStack, MonadReactor c m) => Benign IO J.JSString -> m ()
logInfo m = exec' $ LogLn callStack LogInfo m

-- | @console.warn("WARN  " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
logWarn :: (HasCallStack, MonadReactor c m) => Benign IO J.JSString -> m ()
logWarn m = exec' $ LogLn callStack LogWarn m

-- | @console.error("ERROR " + msg + "[function@path/to/src.hs:line:col in package:Module.Name]")
-- Will print the full callstack, not just the first level.
logError :: (HasCallStack, MonadReactor c m) => Benign IO J.JSString -> m ()
logError m = exec' $ LogLn callStack LogError m

showCmd :: Show c => c -> Benign IO J.JSString
showCmd c = pure (J.pack $ show c)

tracedExec :: (Show cmd, AsFacet cmd c, MonadReactor c m)
    => CallStack -> cmd -> m ()
tracedExec stk c = do
    exec' $ LogLn stk LogTrace (showCmd c)
    exec c

tracedExec' :: (Show (cmd c), AsFacet (cmd c) c, MonadReactor c m)
    => CallStack -> cmd c -> m ()
tracedExec' stk c = do
    exec' $ LogLn stk LogTrace (showCmd c)
    exec' c

tracedEval' :: (Show (cmd c), AsFacet [c] c, AsFacet (cmd c) c, MonadReactor c m)
    => CallStack -> ((a -> c) -> cmd c) -> m a
tracedEval' stk k = eval_ $ tracedExec' stk . k

tracedEval :: (Show cmd, AsFacet [c] c, AsFacet cmd c, MonadReactor c m)
    => CallStack -> ((a -> c) -> cmd) -> m a
tracedEval stk k = eval_ $ tracedExec stk . k

evalBenignIO :: (HasCallStack, MonadReactor c m) => Benign IO a -> m a
evalBenignIO m = delegate $ \fire -> do
    c <- codify fire
    tracedExec' callStack $ EvalBenignIO m c

-- | Make a unique named id
mkReactId :: (HasCallStack, MonadReactor c m)
    => J.JSString -> m ReactId
mkReactId n = delegate $ \fire -> do
    f <- codify fire
    tracedExec' callStack $ MkReactId n f

setRender :: (HasCallStack, MonadReactor c m)
    => WeakObj s -> Window s () -> m ()
setRender obj win = tracedExec' callStack $ SetRender obj win

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkObj :: (HasCallStack, MonadReactor c m)
    => Widget c s s a -> s -> m (Either a (Obj s))
mkObj wid s = delegate $ \fire -> do
    f <- codify fire
    let wid' = wid >>= (instruct . f . Left)
    tracedExec' callStack $ MkObj wid' s (f . Right)

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkObj' :: (HasCallStack, MonadReactor c m)
    => Widget c s s () -> s -> m (Obj s)
mkObj' gad s = delegate $ \fire -> do
    f <- codify fire
    tracedExec' callStack $ MkObj gad s f

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
withMkObj :: (HasCallStack, MonadReactor c m)
    => Widget c s s a -> s -> (Obj s -> m ()) -> m a
withMkObj wid s k = delegate $ \fire -> do
    f <- codify fire
    k' <- codify k
    let wid' = wid >>= (instruct . f)
    tracedExec' callStack $ MkObj wid' s k'

-- | Rerender the ShimComponent using the current @Entity@ context
rerenderObj :: (HasCallStack, MonadReactor c m) => WeakObj o -> m ()
rerenderObj obj = do
    tracedExec' callStack $ Rerender obj

rerender :: (HasCallStack, MonadGadget c o s m) => m ()
rerender = do
    Entity {..} <- ask
    rerenderObj self

-- | Get the 'Model' and exec actions, using the current @Entity@ context
getObjModel :: (HasCallStack, MonadReactor c m) => WeakObj s -> m s
getObjModel obj = delegate $ \fire -> do
    let fire' s = case s of
            Nothing -> pure ()
            Just s' -> fire s'
    c <- codify fire'
    tracedExec' callStack $ EvalBenignIO go c
  where
    go = runMaybeT $ do
        obj' <- benignDeRefWeakObj obj
        lift $ model <$> (benignReadObj obj')

-- | Get the 'Model' and exec actions, using the current @Entity@ context
getModel :: (HasCallStack, MonadGadget c o s m) => m s
getModel = do
    Entity {..} <- ask
    delegate $ \fire -> do
        s <- getObjModel self
        case preview this s of
            Nothing -> pure ()
            Just s' -> fire s'

-- | Get the event target
-- If a "ref" callback to update 'elementalRef' has not been added;
-- then add it, rerender, then return the EventTarget.
getElementalRef :: (HasCallStack, MonadGadget c o s m) => ReactId -> m EventTarget
getElementalRef k = view _weakObj >>= getElementalRefOf k

-- | Get the event target
-- If a "ref" callback to update 'elementalRef' has not been added;
-- then add it, rerender, then return the EventTarget.
getElementalRefOf :: (HasCallStack, MonadReactor c m) => ReactId -> WeakObj o -> m EventTarget
getElementalRefOf k obj = delegate $ \fire -> do
    c <- codify fire
    tracedExec' callStack $ GetElementalRef obj k c

mutateObj :: (HasCallStack, MonadReactor c m) => WeakObj s -> ReactId -> ModelState s () -> m ()
mutateObj obj k m = tracedExec' callStack $ Mutate obj k (command_ <$> m)

-- | Update the 'Model' using the current @Entity@ context
mutate :: (HasCallStack, MonadGadget c o s m) => ReactId -> ModelState s () -> m ()
mutate k m = do
    Entity {..} <- ask
    mutateObj self k (zoom this m)

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
mutateObjThen :: (HasCallStack, Also m a, MonadReactor c m) => WeakObj s -> ReactId -> ModelState s (m a) -> m a
mutateObjThen obj k m = do
    delegate $ \fire -> do
        -- f :: m a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> c
        f' <- codify f
        tracedExec' callStack $ Mutate obj k (f' <$> m)

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
mutateThen :: (HasCallStack, Also m a, MonadGadget c o s m) => ReactId -> ModelState s (m a) -> m a
mutateThen k m = do
    Entity {..} <- ask
    let m' = getAls <$> zoom this (Als <$> m)
    mutateObjThen self k m'

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- Contrast with 'trigger' which expects a 'Notice'.
domTriggerObj ::
    ( HasCallStack
    , NFData a
    , MonadReactor c m
    )
    => WeakObj o
    -> JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
domTriggerObj obj j n goStrict = delegate $ \goLazy -> do
    goLazy' <- codify goLazy
    tracedExec' callStack $ RegisterDOMListener obj j n goStrict goLazy'

domTrigger ::
    ( HasCallStack
    , NFData a
    , MonadGadget c o s m
    )
    => JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
domTrigger j n goStrict = do
    Entity {..} <- ask
    domTriggerObj self j n goStrict

-- | A variation of trigger which ignores the event but fires the given arg instead.
domTrigger_ ::
    (HasCallStack, MonadGadget c o s m)
    => JE.JSRep
    -> J.JSString
    -> a
    -> m a
domTrigger_ j n a = do
    Entity {..} <- ask
    domTriggerObj self j n (const $ pure ())
    pure a

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- Not exposed because all react listeners result in 'Notice'.
triggerObj ::
    ( HasCallStack
    , NFData a
    , MonadReactor c m
    )
    => WeakObj o
    -> ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
triggerObj obj k n goStrict = delegate $ \goLazy -> do
    goLazy' <- codify goLazy
    tracedExec' callStack $ RegisterReactListener obj k n goStrict goLazy'

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
-- Contrast with 'domTrigger' which expects a 'JE.JSRep'.
trigger ::
    ( HasCallStack
    , NFData a
    , MonadGadget c o s m
    )
    => ReactId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger k n goStrict = do
    Entity {..} <- ask
    triggerObj self k n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice g j = MaybeT (pure $ JE.fromJSR j) >>= g

-- | A variation of 'trigger' which ignores the event but fires the given arg instead.
trigger_ ::
    ( HasCallStack
    , MonadGadget c o s m
    )
    => ReactId
    -> J.JSString
    -> a
    -> m a
trigger_ k n a = do
    Entity {..} <- ask
    -- using 'trigger' to bypass conversion to 'Notice' in 'trigger'
    triggerObj self k n (const $ pure ())
    pure a

-- | Register actions to execute after a render.
-- It is safe to 'exec'' a 'Mutate' or 'Rerender'. These command will not
-- trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidMount'
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onObjMounted ::
    (HasCallStack, MonadGadget c o s m)
    => WeakObj o
    -> m a
    -> m a
onObjMounted obj m = delegate $ \fire -> do
    c <- codify' (m >>= fire)
    tracedExec' callStack $ RegisterMountedListener obj c

onMounted ::
    (HasCallStack, MonadGadget c o s m)
    => m a
    -> m a
onMounted m = do
    Entity {..} <- ask
    onObjMounted self m

-- | Register actions to execute after a render.
-- It is safe to 'exec'' a 'Mutate' or 'Rerender'. These command will not
-- trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onObjRendered ::
    (HasCallStack, MonadGadget c o s m)
    => WeakObj o
    -> m a
    -> m a
onObjRendered obj m = delegate $ \fire -> do
    c <- codify' (m >>= fire)
    tracedExec' callStack $ RegisterRenderedListener obj c

onRendered ::
    (HasCallStack, MonadGadget c o s m)
    => m a
    -> m a
onRendered m = do
    Entity {..} <- ask
    onObjRendered self m

-- | Same as 'onRendered' but the action only occurs once on the next rerender.
onObjNextRendered ::
    (HasCallStack, MonadGadget c o s m)
    => WeakObj o -> m a -> m a
onObjNextRendered obj m = delegate $ \fire -> do
    c <- codify' (m >>= fire)
    tracedExec' callStack $ RegisterNextRenderedListener obj c

onNextRendered ::
    (HasCallStack, MonadGadget c o s m)
    => m a -> m a
onNextRendered m = do
    Entity {..} <- ask
    onObjNextRendered self m

-- | Register actions to execute after the state has been updated with 'mutate'.
-- The callback is called` with the 'ReactId' that was passed into 'mutate'.
-- To prevent infinite loops, if this 'onMutated' causes another 'mutate'
-- with the same 'ReactId', then another 'onMutated' callback will NOT be fired.
onObjMutated ::
    (HasCallStack, MonadGadget c o s m)
    => WeakObj o -> (ReactId -> m a) -> m a
onObjMutated obj f = do
    delegate $ \fire -> do
        g <- codify ((fire =<<) . f)
        tracedExec' callStack $ RegisterMutatedListener obj g

onMutated ::
    (HasCallStack, MonadGadget c o s m)
    => (ReactId -> m a) -> m a
onMutated f = do
    Entity {..} <- ask
    onObjMutated self f

-- | Variation of 'onMutated' where you don't care about the 'ReactId' that caused the rerender.
onMutated' ::
    (HasCallStack, MonadGadget c o s m)
    => m a -> m a
onMutated' f = do
    Entity {..} <- ask
    onObjMutated self (const f)

-- | Orphan instance because it requires AsReactor
instance (MonadIO m, A.AToJSON (Benign m) a) => A.AToJSON (Benign m) (Obj a) where
    atoEncoding o = (model <$> benignReadObj o) >>= A.atoEncoding

-- | Specify a default widget for a model type
-- to be used in AFromJSON instances.
-- Use @Tagged t s@ to add other instances, which can be `coerce`d back to @s@.
class DefaultWidget s where
    defaultWidget :: Widget c s s ()

-- | Use @Obj (Tagged t s)@ which is 'coerce'-able to @Obj s@ to use other instances of 'defaultWidget'
instance (MonadReactor c m, DefaultWidget a, A.AFromJSON m a) => A.AFromJSON m (Obj a) where
    aparseJSON v = do
        ma <- A.aparseJSON v
        pure (ma >>= mkObj' defaultWidget)
