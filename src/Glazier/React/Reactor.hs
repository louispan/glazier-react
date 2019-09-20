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
import Data.String
import Data.Tagged.Extras
import GHC.Stack.Extras
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

-- | A 'MonadGadget'' is can log, 'instruct' 'Reactor' effects,
-- and can that can be safely turned into a 'Handler'
-- and used in event handling code.
-- It is an instance of 'Alternative' and 'Also' so it can be combined.
type MonadGadget' c m = (Cmd' [] c, CmdReactor c
        , MonadLogger J.JSString c m, AskLogName m, AskReactPath m
        , AskPlanWeakRef m
        , Alternative m, Also () m
        )

-- | A 'MonadGadget' with an addition type @s@ parameter for modifying the model
type MonadGadget s c m = (MonadGadget' c m, AskModelWeakVar s m)

-- A 'MonadGadget' that additionally have access to 'onConstruction', 'onDestruction', 'onRendered',
-- can generate 'Markup' and so should not be be for event handling, sice those
-- additional effects are ignored inside event handling.
type MonadWidget' c m = (MonadGadget' c m, PutMarkup m, PutReactPath m
        , AskConstructor c m, AskDestructor c m, AskRendered c m)

-- A 'MonadWidget'' with an addition type @s@ parameter for displaying the model
type MonadWidget s c m = (MonadWidget' c m, AskModelWeakVar s m, AskModel s m)

-- | Describes the effects required by 'Widget' to manipulate 'Obj'.
-- 'Reactor' is not a functor because of the @Widget c@ in 'MkObj' which
-- is in a positive agument position.
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
    MkListener ::
        Weak (IORef Plan)
        -> Handler
        -> (Listener -> c)
        -> Reactor c

    -- | Make a fully initialized object from a widget and meta
    -- 'Reactor' is not a functor because of the @Widget c@ in 'MkObj' which
    -- is in a positive agument position.
    MkObj :: Widget s c() -> LogName -> s -> (Obj s -> c) -> Reactor c

    -- | Reads from an 'Obj', also registering this as a listener
    -- so this will get rerendered whenever the 'Obj' is 'mutate'd.
    ReadObj :: Weak (IORef Plan) -> Obj s -> (s -> c) -> Reactor c

    -- Modifies the model and flags 'RerenderRequired'
    -- Also notifier any watchers that the model has changes
    -- so that the watchers can rerender
    Mutate :: Weak (IORef Plan) -> Weak (MVar s) -> RerenderRequired -> StateT s IO c -> Reactor c


instance (IsString str, Semigroup str) => ShowIO str (Reactor c) where
    showsPrecIO p (MkHandler this _ _ _) = showParenIO (p >= 11) $ (showStr "MkHandler " .) <$> (showsIO this)
    showsPrecIO p (MkListener this _ _) = showParenIO (p >= 11) $ (showStr "MkListener " .) <$> (showsIO this)
    showsPrecIO p (MkObj _ logname _ _) = showParenIO (p >= 11) $ (showStr "MkObj " .) <$> (showsIO logname)
    showsPrecIO p (ReadObj this _ _) = showParenIO (p >= 11) $ (showStr "ReadObj " .) <$> (showsIO this)
    showsPrecIO p (Mutate this _ req _) = showParenIO (p >= 11) $ (\x -> (showStr "Mutate ") . x . (showFromStr " ") . (showsStr req)) <$> (showsIO this)

logPrefix :: AskReactPath m => m J.JSString
logPrefix = do
    ps <- getReactPath <$> askReactPath
    let xs = DL.intersperse "." $ (\(n, i) -> n <> (fromString $ show i)) <$> ps
    pure (foldr (<>) "" xs)

loggedJS :: (HasCallStack, ShowIOJS a, MonadGadget' c m) => (a -> m b) -> LogLevel -> a -> m b
loggedJS go lvl a = withoutCallStack $ do
    logLnJS lvl (showIO a)
    go a

logLnJS :: (HasCallStack, MonadGadget' c m)
    => LogLevel -> IO J.JSString
    -> m ()
logLnJS lvl msg = withoutCallStack $ do
    p <- logPrefix
    n <- untag' @"LogName" <$> askLogName
    logLine basicLogCallStackDepth lvl $ (\x -> n <> "<" <> p <> "> " <> x) <$> msg


------------------------------------------------------
-- Basic
------------------------------------------------------

-- | Make an initialized 'Obj' for a given meta using the given 'Widget'.
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj :: (HasCallStack, MonadGadget' c m)
    => Widget s c () -> LogName -> s -> m (Obj s)
mkObj wid logname s = delegatify $ \f ->
    loggedJS exec' TRACE $ MkObj wid logname s f

-- | Convenient variation of 'mkObj' where the widget is unlifted from the given monad.
-- This is useful for transformer stacks that require addition MonadReader-like effects.
unliftMkObj :: (HasCallStack, MonadUnliftWidget s c m, MonadGadget' c m)
    => m () -> LogName -> s -> m (Obj s)
unliftMkObj m logname s = do
    u <- askUnliftWidget
    mkObj (unliftWidget u m) logname s

-- | Reads from an 'Obj', also registering this as a listener
-- so this will get rerendered whenever the 'Obj' is 'mutate'd.
readObj :: (HasCallStack, MonadGadget' c m)
    => Obj s -> m s
readObj obj = do
    this <- askPlanWeakRef
    delegatify $ \f ->
        loggedJS exec' TRACE $ ReadObj this obj f

-- | 'mutate_' with 'RerenderRequired'
mutate :: (HasCallStack, MonadGadget s c m)
    => StateT s IO () -> m ()
mutate = mutate_ RerenderRequired

-- | Mutates the Model for the current widget.
-- Expose 'Rerender' for "uncontrolled (by react)" components like <input>
-- which doesn't necessarily require a rerender if the model changes.
-- Mutation notifications are always sent.
mutate_ :: ( HasCallStack, MonadGadget s c m)
    => RerenderRequired -> StateT s IO () -> m ()
mutate_ r m = do
    mdl <- askModelWeakVar
    this <- askPlanWeakRef
    loggedJS exec' TRACE $ Mutate this mdl r (command_ <$> m)

-- | 'mutateThen_' with 'RerenderRequired'
mutateThen :: ( HasCallStack, MonadGadget s c m)
    => StateT s IO (m a) -> m a
mutateThen = mutateThen_ RerenderRequired

-- | Variation of 'mutate_' which also returns the next action to execute after mutating.
mutateThen_ :: ( HasCallStack, MonadGadget s c m)
    => RerenderRequired -> StateT s IO (m a) -> m a
mutateThen_ r m = do
    mdl <- askModelWeakVar
    this <- askPlanWeakRef
    delegate $ \fire -> do
        -- f :: m a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> c
        f' <- codify f
        loggedJS exec' TRACE $ Mutate this mdl r (f' <$> m)

-- | This convert the input @goStrict@ and @f@ into (preprocess, postprocess).
-- Multiple preprocess must be run for the same event before any of the postprocess,
-- due to the way ghcjs sync and async threads interact with React js.
mkHandler ::
    (HasCallStack, NFData a, MonadGadget' c m)
    => (J.JSVal -> MaybeT IO a)
    -> (a -> m ())
    -> m (J.JSVal -> IO (), IO ()) -- (preprocess, postprocess)
mkHandler goStrict f = do
    plnRef <- askPlanWeakRef
    f' <- codify f
    delegatify $ \k -> do
        loggedJS exec' TRACE $ MkHandler plnRef goStrict f' k

handleNotice :: Monad m => (Notice -> MaybeT m a) -> (J.JSVal -> MaybeT m a)
handleNotice g j = MaybeT (pure $ JE.fromJS j) >>= g

mkNoticeHandler ::
    (HasCallStack, NFData a, MonadGadget' c m)
    => (Notice -> MaybeT IO a)
    -> (a -> m ())
    -> m (J.JSVal -> IO (), IO ()) -- (preprocess, postprocess)
mkNoticeHandler goStrict goLazy = mkHandler (handleNotice goStrict) goLazy

-- | This convert 'Handler' into a ghcjs 'Callback'
mkListener ::
    (HasCallStack, MonadGadget' c m)
    => Handler
    -> m Listener
mkListener f = do
    plnRef <- askPlanWeakRef
    delegatify $ \k ->
        loggedJS exec' TRACE $ MkListener plnRef f k

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
rawTxt :: MonadWidget s c m => Getting J.JSString s J.JSString -> m ()
rawTxt lns = do
    s <- askModel
    rawTextMarkup $ s ^. lns

lf :: MonadWidget s c m
    => J.JSString-- ^ eg "div" or "input"
    -> DL.DList (J.JSString, m Handler)
    -> DL.DList (J.JSString, Getting J.JSVal s J.JSVal)
    -> m ()
lf n gads props = do
    s <- askModel
    putNextReactPath n
    gads' <- traverse sequenceA (DL.toList gads) -- :: m [(JString, Handler)]
    let gads'' = M.fromListWith (<>) gads' -- combine same keys together
    gads''' <- traverse mkListener gads'' -- convert to JS callback
    leafMarkup (JE.toJS n)
        (((fmap (`view` s)) <$> props)
            <> (DL.fromList . M.toList $ JE.toJS <$> gads'''))

bh :: MonadWidget s c m
    => J.JSString-- ^ eg "div" or "input"
    -> DL.DList (J.JSString, m Handler)
    -> DL.DList (J.JSString, Getting J.JSVal s J.JSVal)
    -> m a
    -> m a
bh n gads props child = do
    s <- askModel
    putNextReactPath n
    gads' <- traverse sequenceA (DL.toList gads) -- :: m [(JString, Handler)]
    let gads'' = M.fromListWith (<>) gads' -- combine same keys together
    gads''' <- traverse mkListener gads'' -- convert to JS callback
    putPushReactPath
    a <- branchMarkup (JE.toJS n)
        (((fmap (`view` s)) <$> props)
            <> (DL.fromList . M.toList $ JE.toJS <$> gads'''))
        child
    putPopReactPath
    pure a
