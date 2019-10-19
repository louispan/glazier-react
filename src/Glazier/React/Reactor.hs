{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import qualified Data.JSString as J
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import Data.Tagged.Extras
import GHC.Stack
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.DOM.Event
import Glazier.DOM.EventTarget
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Component
import Glazier.React.Markup
import Glazier.React.Obj.Internal
import Glazier.React.Plan
import Glazier.React.ReactPath
import Glazier.React.Widget
import qualified JavaScript.Extras as JE
import System.Mem.Weak

-----------------------------------------------------------------

type CmdReactor c =
    ( Cmd' [] c -- required by 'command_'
    , Cmd' IO c -- required by 'MonadCodify' for @ProgramT IO@
    , Cmd' Reactor c
    , Cmd (LogLine J.JSString) c
    )

-- | A 'MonadGadget'' is can log, 'instruct' 'Reactor' effects,
-- and can that can be safely turned into a 'Handler'
-- and used in event handling code.
-- It is an instance of 'Alternative' and 'Also' so it can be combined.
class (CmdReactor (Command m)
    , AlternativeIO m, Also () m
    , MonadLogger J.JSString m, AskLogName m, AskReactPath m
    , AskPlanWeakRef m, AskModel s m, AskModelWeakVar s m) => MonadGadget s m

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadGadget s m
    , Command (t m) ~ Command m
    , AlternativeIO (t m), Also () (t m)
    , MonadCommand (t m)
    ) => MonadGadget s (t m)

instance {-# OVERLAPPABLE #-} (CmdReactor c, c ~ Command (Widget s c)) => MonadGadget s (Widget s c)

-- A 'MonadWidget' is a 'MonadGadget' that additionally have access to
-- 'initConstructor', 'initDestructor', 'initRendered',
-- can generate 'Markup' and so should not be be for event handling, sice those
-- additional effects are ignored inside event handling.
class (CmdReactor (Command m)
    , MonadGadget s m, PutMarkup m, PutReactPath m
    , AskConstructor m, AskDestructor m, AskRendered m) => MonadWidget s m

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadWidget s m
    , Command (t m) ~ Command m
    , AlternativeIO (t m), Also () (t m)
    , MonadCommand (t m)
    ) => MonadWidget s (t m)

instance {-# OVERLAPPABLE #-} (CmdReactor c, c ~ Command (Widget s c)) => MonadWidget s (Widget s c)

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
    MkObj :: Widget s c() -> LogName -> ModelVar s -> (Obj s -> c) -> Reactor c

    -- | Reads from an 'Obj', also registering this as a listener
    -- so this widget will get rerendered whenever the 'Obj' is 'mutate'd.
    ReadWeakObj :: Weak (IORef Plan) -> WeakObj s -> (s -> c) -> Reactor c

    -- | Deregister from reading an 'Obj' so that this widget will *not* get
    -- rerendered if th 'Obj' is mutated
    UnreadWeakObj :: Weak (IORef Plan) -> WeakObj s -> Reactor c

    -- Modifies the model and flags 'RerenderRequired' for this widget.
    -- Irregardless of 'RerenderRequired', it will notifier any watchers (from 'ReadObj')
    -- that the model has changed so that the watchers can rerender.
    Mutate :: Weak (IORef Plan) -> Weak (MVar s) -> RerenderRequired -> StateT s IO c -> Reactor c


-- instance (IsString str, Semigroup str) => ShowIO str (Reactor c) where
--     showsPrecIO p (MkHandler this _ _ _) = showParenIO (p >= 11) $ (showStr "MkHandler " .) <$> (showsIO this)
--     showsPrecIO p (MkListener this _ _) = showParenIO (p >= 11) $ (showStr "MkListener " .) <$> (showsIO this)
--     showsPrecIO p (MkObj _ logname _ _) = showParenIO (p >= 11) $ (showStr "MkObj " .) <$> (showsIO logname)
--     showsPrecIO p (ReadObj this _ _) = showParenIO (p >= 11) $ (showStr "ReadObj " .) <$> (showsIO this)
--     showsPrecIO p (Mutate this _ req _) = showParenIO (p >= 11) $ (\x -> (showStr "Mutate ") . x . (showFromStr " ") . (showsStr req)) <$> (showsIO this)

logPrefix :: MonadGadget s m => m J.JSString
logPrefix = do
    ps <- getReactPath <$> askReactPath
    let xs = DL.intersperse "." $ (\(n, i) -> n <> (fromString $ show i)) <$> ps
    pure (foldr (<>) "" xs)

-- loggedJS :: (HasCallStack, ShowIOJS a, MonadGadget' c m) => (a -> m b) -> LogLevel -> a -> m b
-- loggedJS go lvl a = withoutCallStack $ do
--     logLnJS lvl (showIO a)
--     go a

logJS :: (HasCallStack, MonadGadget s m)
    => LogLevel -> IO J.JSString
    -> m ()
logJS lvl msg = withFrozenCallStack $ do
    p <- logPrefix
    n <- untag' @"LogName" <$> askLogName
    logLine basicLogCallStackDepth lvl $ (\x -> n <> "<" <> p <> "> " <> x) <$> msg

------------------------------------------------------
-- Basic
------------------------------------------------------

-- | Make a 'MVar' and 'Weak' 'MVar' for a model.
-- This is used if you want to share the same model between different widgets.
mkModelVar :: MonadIO m => s -> m (ModelVar s)
mkModelVar s = liftIO $ do
    mdlVar <- newMVar s
    mdlWkVar <- mkWeakMVar mdlVar (pure ())
    pure $ Ref mdlVar mdlWkVar

-- | Make an initialized 'Obj' for a given meta using the given 'Widget' and 'ModelVar'
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
-- The same 'ModelVar' can be used for different 'mkObj'
mkObj :: MonadGadget s m => Widget t (Command m) () -> LogName -> ModelVar t -> m (Obj t)
mkObj wid logname s = delegatify $ exec' . MkObj wid logname s

-- | This does 'mkModelVar' and 'mkObj' in one step.
mkObj' :: MonadGadget s m  => Widget t (Command m) () -> LogName -> t -> m (Obj t)
mkObj' wid logname s = mkModelVar s >>= mkObj wid logname

-- | Convenient variation of 'mkObj' where the widget is unlifted from the given monad.
-- This is useful for transformer stacks that require addition MonadReader-like effects.
-- The same 'ModelVar' can be used for different 'unliftMkObj'
unliftMkObj :: (MonadUnliftWidget s m, MonadGadget s m)
    => m () -> LogName -> ModelVar s -> m (Obj s)
unliftMkObj m logname s = do
    u <- askUnliftWidget
    mkObj (unliftWidget u m) logname s

-- | 'mkModelVar' and 'unliftMkObj' in one step
unliftMkObj' :: (MonadUnliftWidget s m, MonadGadget s m)
    => m () -> LogName -> s -> m (Obj s)
unliftMkObj' m logname s = mkModelVar s >>= unliftMkObj m logname

-- | Reads from an 'Obj', also registering this as a listener
-- so this will get rerendered whenever the 'Obj' is 'mutate'd.
readWeakObj :: MonadGadget s m => WeakObj t -> m t
readWeakObj obj = do
    this <- askPlanWeakRef
    delegatify $ exec' . ReadWeakObj this obj

-- | Reads from an 'Obj', also registering this as a listener
-- so this will get rerendered whenever the 'Obj' is 'mutate'd.
unreadWeakObj :: MonadGadget s m => WeakObj t -> m ()
unreadWeakObj obj = do
    this <- askPlanWeakRef
    exec' $ UnreadWeakObj this obj

-- | 'mutate' with 'RerenderRequired'
mutate' :: (MonadGadget s m) => StateT s IO a -> m a
mutate' = mutate RerenderRequired

-- | Mutates the Model for the current widget.
-- Expose 'Rerender' for "uncontrolled (by react)" components like <input>
-- which doesn't necessarily require a rerender if the model changes.
-- Mutation notifications are always sent.
mutate :: MonadGadget s m => RerenderRequired -> StateT s IO a -> m a
mutate r m = do
    mdl <- askModelWeakVar
    this <- askPlanWeakRef
    delegatify $ \f -> exec' $ Mutate this mdl r (f <$> m)

-- | This convert the input @goStrict@ and @f@ into (preprocess, postprocess).
-- Multiple preprocess must be run for the same event before any of the postprocess,
-- due to the way ghcjs sync and async threads interact with React js.
mkHandler ::
    (NFData a, MonadGadget s m)
    => (J.JSVal -> MaybeT IO a)
    -> (a -> m ())
    -> m Handler -- (preprocess, postprocess)
mkHandler goStrict f = do
    plnRef <- askPlanWeakRef
    f' <- codify f
    delegatify $ exec' . MkHandler plnRef goStrict f'

handleSyntheticEvent :: Monad m => (SyntheticEvent -> MaybeT m a) -> (J.JSVal -> MaybeT m a)
handleSyntheticEvent g j = MaybeT (pure $ JE.fromJS j) >>= g

mkSyntheticHandler ::
    (NFData a, MonadGadget s m)
    => (SyntheticEvent -> MaybeT IO a)
    -> (a -> m ())
    -> m Handler
mkSyntheticHandler goStrict goLazy = mkHandler (handleSyntheticEvent goStrict) goLazy

-- | This convert 'Handler' into a ghcjs 'Callback'
mkListener ::
    (MonadGadget s m)
    => Handler
    -> m Listener
mkListener f = do
    plnRef <- askPlanWeakRef
    delegatify $ exec' . MkListener plnRef f

-- | Add a listener with an event target, and automatically removes it on widget destruction
-- This only does something during initialization
listenEventTarget :: (NFData a, MonadWidget s m, IEventTarget j)
    => j -> J.JSString -> (J.JSVal -> MaybeT IO a) -> (a -> m ()) -> m ()
listenEventTarget j n goStrict goLazy =
    initConstructor $ do
        hdl <- mkHandler goStrict goLazy
        cb <- mkListener hdl
        liftIO $ addEventListener j n cb
        initDestructor $ liftIO $ removeEventListener j n cb

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

-- | write some text from the model.
txt :: MonadWidget s m => (s -> J.JSString) -> m ()
txt f = do
    s <- askModel
    textMarkup $ f s

-- | This orphan instance allows using "blah" is a prop in 'txt'
-- when using @OverloadedString@ with @ExtendedDefaultRules@
instance IsString (s -> J.JSString) where
    fromString = const . J.pack

type Prop s = s -> Maybe J.JSVal

-- | This orphan instance allows using "blah" is a prop in 'lf' and 'bh'
-- when using @OverloadedString@ with @ExtendedDefaultRules@
instance IsString (Prop s) where
    fromString = const . Just . JE.toJS

-- | Creates a JE.JSRep single string for "className" property from a list of (JSString, Bool)
-- Idea from https://github.com/JedWatson/classnames
classNames :: [(J.JSString, s -> Maybe Bool)] -> Prop s
classNames xs s =
    Just
    . JE.toJS
    . J.unwords
    . fmap fst
    . filter (fromMaybe False . snd)
    $ (fmap ($ s)) <$> xs

lf :: (Component j, MonadWidget s m)
    => j -- ^ "input" or a @ReactComponent@
    -> DL.DList (J.JSString, m Handler)
    -> DL.DList (J.JSString, Prop s)
    -> m ()
lf j gads props = do
    s <- askModel
    let props' = (fmap (fromMaybe J.nullRef . ($ s))) <$> props
    putNextReactPath (componentName j)
    gads' <- traverse sequenceA (DL.toList gads) -- :: m [(JString, Handler)]
    let gads'' = M.fromListWith (<>) gads' -- combine same keys together
    gads''' <- traverse mkListener gads'' -- convert to JS callback
    leafMarkup (JE.toJS j)
        (props' <> (DL.fromList . M.toList $ JE.toJS <$> gads'''))

bh :: (Component j, MonadWidget s m)
    => j-- ^ eg "div" or a @ReactComponent@
    -> DL.DList (J.JSString, m Handler)
    -> DL.DList (J.JSString, Prop s)
    -> m a
    -> m a
bh j gads props child = do
    s <- askModel
    let props' = (fmap (fromMaybe J.nullRef . ($ s))) <$> props
    putNextReactPath (componentName j)
    gads' <- traverse sequenceA (DL.toList gads) -- :: m [(JString, Handler)]
    let gads'' = M.fromListWith (<>) gads' -- combine same keys together
    gads''' <- traverse mkListener gads'' -- convert to JS callback
    putPushReactPath
    a <- branchMarkup (JE.toJS j)
        (props' <> (DL.fromList . M.toList $ JE.toJS <$> gads'''))
        child
    putPopReactPath
    pure a

