{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import qualified Data.JSString as J
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.String
import Data.Tagged.Extras
import GHC.Stack
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Markup
import Glazier.React.Notice
import Glazier.React.Obj
import Glazier.React.Plan
import Glazier.React.ReactPath
import Glazier.React.Widget
import Glazier.ShowIO
import qualified JavaScript.Extras as JE
import System.Mem.Weak

-----------------------------------------------------------------
type CmdReactor c =
    ( Cmd' [] c -- required by 'command_'
    , Cmd' Reactor c
    , Cmd (LogLine J.JSString) c
    )

type LoggerJS c m = (Logger J.JSString c m, AskLogName m, AskReactPath m)

-- | Doesn't need to know about the model @s@
type MonadReactor c m = (Cmd' [] c, CmdReactor c, Alternative m, Also () m, LoggerJS c m)

-- | Needs to know about the model @s@ and also modifies markup
-- type MonadWidget c s m = (MonadGadget c s m, AskModelWeakRef s m) --, PutReactId m, PutMarkup m)

-- | NB. 'Reactor' is not a functor because of the @Widget c@ in 'MkObj'
data Reactor c where

    -- Turn some handling function into a 'Handler'.
    -- The reason for the two handling functions is detailed in 'Glazier.React.Reactor.Exec.execMkHandler'
    -- Glazier will try to return the same 'Handler for the same input functions as much as possible
    -- so that it will be relatively efficient to use this function on every rerender.
    MkHandler :: NFData a
        => Weak (IORef Plan)
        -> (J.JSVal -> MaybeT IO a)
        -> (a -> c)
        -> (Handler -> c)
        -> Reactor c

    -- | Turn 'Handler' into a 'J.Callback' so it can be called from JS.
    -- Glazier will try to return the same 'J.Callback' for the same input functions as much as possible.
    -- so that it will be relatively efficient to use this function on every rerender.
    MkCallback ::
        Weak (IORef Plan)
        -> Handler
        -> (J.Callback (J.JSVal -> IO ()) -> c)
        -> Reactor c

    -- | Make a fully initialized object from a widget and meta
    -- MkObj :: Widget c s s () -> J.JSString -> s -> (Obj s -> c) -> Reactor c
    -- NB. 'Reactor' is not a functor because of the @Widget c@ in 'MkObj'
    MkObj :: Widget c s () -> LogName -> s -> (Obj s -> c) -> Reactor c

    -- | Reads from an 'Obj', also registering this as a listener
    -- so this will get rerendered whenever the 'Obj' is 'mutate'd.
    ReadObj :: Weak (IORef Plan) -> Obj s -> (s -> c) -> Reactor c

    -- Modifies the model and flags 'RerenderRequired'
    -- Also notifier any listener that the model has changes
    -- so that the listeners can rerender
    Mutate :: Weak (IORef Plan) -> Weak (MVar s) -> RerenderRequired -> StateT s IO c -> Reactor c

    -- -- | Create and register a dom callback
    -- -- Returns a command that can be used to explicitly remove the dom listener.
    -- -- Dom listeners are automatically removed on widget destruction.
    -- AddDomListener :: NFData a
    --     => WeakModelRef s
    --     -> J.JSVal
    --     -> J.JSString
    --     -> (J.Callback (J.JSVal -> IO ()) -> c)
    --     -> (c -> a -> c)
    --     -> Reactor c

    -- -- | Remove a DOM listener
    -- RemoveDomListener ::
    --     => WeakModelRef s
    --     -> ReactId
    --     -> Reactor c


instance (IsString str, Semigroup str) => ShowIO str (Reactor c) where
    -- showsPrecIO p (MkReactId s _) = textParen (p >= 11) $
    --     "MkReactId " <> showIO s
    showsPrecIO p (MkHandler this _ _ _) = showParenIO (p >= 11) $ (showStr "MkHandler " .) <$> (showsIO this)
    showsPrecIO p (MkCallback this _ _) = showParenIO (p >= 11) $ (showStr "MkCallback " .) <$> (showsIO this)
    showsPrecIO p (MkObj _ logname _ _) = showParenIO (p >= 11) $ (showStr "MkObj " .) <$> (showsIO logname)
    showsPrecIO p (ReadObj this _ _) = showParenIO (p >= 11) $ (showStr "ReadObj " .) <$> (showsIO this)
    -- showsPrec _ (SetRender _ _ ) = showString "SetRender"
    -- showsPrec _ (GetReactRef _ _ _) = showString "GetReactRef"
    -- showsPrecIO p (Rerender this) = showParenIO (p >= 11) $ (showStr "Rerender " .) <$> (showsIO this)
    showsPrecIO p (Mutate this _ req _) = showParenIO (p >= 11) $ (\x -> (showStr "Mutate ") . x . (showFromStr " ") . (showFromStr $ show req)) <$> (showsIO this)
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


logPrefix :: AskReactPath m => m J.JSString
logPrefix = do
    ps <- getReactPath <$> askReactPath
    let xs = DL.intersperse "." $ (\(n, i) -> n <> (fromString $ show i)) <$> ps
    pure (foldr (<>) "" xs)

logLineJS :: LoggerJS c m
    => LogLevel -> CallStack -> IO J.JSString
    -> m ()
logLineJS lvl cs msg = do
    p <- logPrefix
    n <- untag' @"LogName" <$> askLogName
    logLine (Proxy @J.JSString) lvl cs $ (\x -> n <> "<" <> p <> "> " <> x) <$> msg

logExecJS :: (ShowIOJS cmd, Cmd cmd c, LoggerJS c m)
    => LogLevel -> CallStack -> cmd -> m ()
logExecJS = logExec (Proxy @J.JSString)

logExecJS' :: (ShowIOJS (cmd c), Cmd' cmd c, LoggerJS c m)
    => LogLevel -> CallStack -> cmd c -> m ()
logExecJS' = logExec' (Proxy @J.JSString)

logEvalJS :: (ShowIOJS cmd, Cmd cmd c, LoggerJS c m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd) -> m a
logEvalJS = logEval (Proxy @J.JSString)

logEvalJS' :: (ShowIOJS (cmd c), Cmd' cmd c, LoggerJS c m)
    => LogLevel -> CallStack -> ((a -> c) -> cmd c) -> m a
logEvalJS' = logEval' (Proxy @J.JSString)

logInvokeJS :: (ShowIOJS (cmd c), Cmd' cmd c, Functor cmd, LoggerJS c m)
    => LogLevel -> CallStack -> cmd a -> m a
logInvokeJS = logInvoke (Proxy @J.JSString)

logInvokeJS_ :: (ShowIOJS (cmd c), Cmd' cmd c, Cmd' [] c, Functor cmd, LoggerJS c m)
    => LogLevel -> CallStack -> cmd () -> m ()
logInvokeJS_ = logInvoke_ (Proxy @J.JSString)

------------------------------------------------------
-- Basic
------------------------------------------------------

-- | Make an initialized 'Obj' for a given meta using the given 'Widget'.
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj :: (HasCallStack, MonadReactor c m)
    => Widget c s () -> LogName -> s -> m (Obj s)
mkObj wid logname s = delegatify $ \f ->
    logExecJS' TRACE callStack $ MkObj wid logname s f

-- | Convenient variation of 'mkObj' where the widget is unlifted from the given monad.
-- This is useful for transformer stacks that require addition MonadReader-like effects.
unliftMkObj :: (HasCallStack, MonadUnliftWidget c s m, MonadReactor c m)
    => m () -> LogName -> s -> m (Obj s)
unliftMkObj m logname s = do
    u <- askUnliftWidget
    mkObj (unliftWidget u m) logname s

-- | Reads from an 'Obj', also registering this as a listener
-- so this will get rerendered whenever the 'Obj' is 'mutate'd.
readObj :: (HasCallStack, MonadReactor c m, AskPlanWeakRef m)
    => Obj s -> m s
readObj obj = do
    this <- askPlanWeakRef
    delegatify $ \f ->
        logExecJS' TRACE callStack $ ReadObj this obj f

-- | 'mutate_' with 'RerenderRequired'
mutate :: ( HasCallStack, MonadReactor c m, AskModelWeakVar s m, AskPlanWeakRef m)
    => StateT s IO () -> m ()
mutate = mutate_ RerenderRequired

-- | Mutates the Model for the current widget.
-- Expose 'Rerender' for "uncontrolled (by react)" components like <input>
-- which doesn't necessarily require a rerender if the model changes.
-- Mutation notifications are always sent.
mutate_ :: ( HasCallStack, MonadReactor c m, AskModelWeakVar s m, AskPlanWeakRef m)
    => RerenderRequired -> StateT s IO () -> m ()
mutate_ r m = do
    mdl <- askModelWeakVar
    this <- askPlanWeakRef
    logExecJS' TRACE callStack $ Mutate this mdl r (command_ <$> m)

-- | 'mutateThen_' with 'RerenderRequired'
mutateThen :: ( HasCallStack, MonadReactor c m, AskModelWeakVar s m, AskPlanWeakRef m)
    => StateT s IO (m a) -> m a
mutateThen = mutateThen_ RerenderRequired

-- | Variation of 'mutate_' which also returns the next action to execute after mutating.
mutateThen_ :: ( HasCallStack, MonadReactor c m, AskModelWeakVar s m, AskPlanWeakRef m)
    => RerenderRequired -> StateT s IO (m a) -> m a
mutateThen_ r m = do
    mdl <- askModelWeakVar
    this <- askPlanWeakRef
    delegate $ \fire -> do
        -- f :: m a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> c
        f' <- codify f
        logExecJS' TRACE callStack $ Mutate this mdl r (f' <$> m)

-- -- | Create a callback for a 'J.JSVal' and add it to this elementals's dlist of listeners.
-- -- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- -- Contrast with 'trigger' which expects a 'Notice'.
-- domTrigger ::
--     (HasCallStack, NFData a, AsReactor c, MonadGadget' c s m)
--     => J.JSVal
--     -> J.JSString
--     -> (J.JSVal -> MaybeT IO a)
--     -> m a
-- domTrigger j n goStrict = do
--     obj <- askWeakModelRef
--     delegatify $ \f ->
--         logExec' TRACE callStack $ RegisterDOMListener obj j n goStrict f

-- -- | A variation of trigger which ignores the event but fires the given arg instead.
-- -- Unlike 'domTrigger' the @a@ does not need to be @NFData a@
-- domTrigger_ ::
--     (HasCallStack, AsReactor c, MonadGadget' c s m)
--     => J.JSVal
--     -> J.JSString
--     -> a
--     -> m a
-- domTrigger_ j n a = do
--     runGadget $ domTrigger j n (const $ pure ())
--     pure a



-- -- | Create a callback for a 'J.JSVal' and add it to this elementals's dlist of listeners.
-- -- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- -- Contrast with 'trigger' which expects a 'Notice'.
-- addDomListener ::
--     (HasCallStack, NFData a, MonadReactor c m, AskPlanWeakRef m)
--     => J.JSVal
--     -> J.JSString
--     -> (J.JSVal -> MaybeT IO a)
--     -> (a -> m ())
--     -> m c
-- addDomListener j n goStrict goLazy = do
--     plnRef <- askPlanWeakRef
--     delegatify $ \f ->
--         logExec' TRACE callStack $ RegisterDOMListener obj j n goStrict f


-- | This convert the (preprocess, postprocess) into a ghcjs 'Callback'
mkCallback ::
    (HasCallStack, MonadReactor c m, AskPlanWeakRef m)
    => (J.JSVal -> IO (), IO ())
    -> m (J.Callback (J.JSVal -> IO ()))
mkCallback f = do
    plnRef <- askPlanWeakRef
    delegatify $ \k ->
        logExecJS' TRACE callStack $ MkCallback plnRef f k

-- | This convert the input @goStrict@ and @f@ into (preprocess, postprocess).
-- Multiple preprocess must be run for the same event before any of the postprocess,
-- due to the way ghcjs sync and async threads interact with React js.
mkHandler ::
    (HasCallStack, NFData a, MonadReactor c m, AskPlanWeakRef m)
    => (J.JSVal -> MaybeT IO a)
    -> (a -> m ())
    -> m (J.JSVal -> IO (), IO ()) -- (preprocess, postprocess)
mkHandler goStrict f = do
    plnRef <- askPlanWeakRef
    f' <- codify f
    delegatify $ \k -> do
        logExecJS' TRACE callStack $ MkHandler plnRef goStrict f' k

handleNotice :: Monad m => (Notice -> MaybeT m a) -> (J.JSVal -> MaybeT m a)
handleNotice g j = MaybeT (pure $ JE.fromJS j) >>= g

mkNoticeHandler ::
    (HasCallStack, NFData a, MonadReactor c m, AskPlanWeakRef m)
    => (Notice -> MaybeT IO a)
    -> (a -> m ())
    -> m (J.JSVal -> IO (), IO ()) -- (preprocess, postprocess)
mkNoticeHandler goStrict goLazy = mkHandler (handleNotice goStrict) goLazy

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

-- | write some text from the model. Use 'like' to 'const' a string instead.
rawTxt :: (PutMarkup m, AskModel s m) => Getting J.JSString s J.JSString -> m ()
rawTxt lns = do
    s <- askModel
    rawTextMarkup $ s ^. lns

lf ::
    ( PutMarkup m
    , PutReactPath m
    , AskModel s m
    , AskPlanWeakRef m
    , MonadReactor c m
    )
    => J.JSString-- ^ eg "div" or "input"
    -> DL.DList (J.JSString, m Handler)
    -> DL.DList (J.JSString, Getting J.JSVal s J.JSVal)
    -> m ()
lf n gads props = do
    s <- askModel
    putNextReactPath n
    gads' <- traverse sequenceA (DL.toList gads)
    let gads'' = M.fromListWith (<>) gads'
    gads''' <- traverse mkCallback gads''
    leafMarkup (JE.toJS n)
        (((fmap (`view` s)) <$> props)
            <> (DL.fromList . M.toList $ JE.toJS <$> gads'''))

bh ::
    ( PutMarkup m
    , PutReactPath m
    , AskModel s m
    , AskPlanWeakRef m
    , MonadReactor c m
    )
    => J.JSString-- ^ eg "div" or "input"
    -> DL.DList (J.JSString, m Handler)
    -> DL.DList (J.JSString, Getting J.JSVal s J.JSVal)
    -> m a
    -> m a
bh n gads props child = do
    s <- askModel
    putNextReactPath n
    gads' <- traverse sequenceA (DL.toList gads)
    let gads'' = M.fromListWith (<>) gads'
    gads''' <- traverse mkCallback gads''
    putPushReactPath
    a <- branchMarkup (JE.toJS n)
        (((fmap (`view` s)) <$> props)
            <> (DL.fromList . M.toList $ JE.toJS <$> gads'''))
        child
    putPopReactPath
    pure a
