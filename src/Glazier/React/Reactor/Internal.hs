{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor.Internal where

import Control.Also
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import Data.IORef.Extras
import qualified Data.JSString as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Markup
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.ReactId
import Glazier.React.ReactPath
import Glazier.React.Widget
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
    , AskScratch m, AskPlanWeakRef m
    , AskNotifierWeakRef m, AskModel s m, AskModelWeakVar s m) => MonadGadget s m

-- -- | Any transformer on top of MonadGadget is also a MonadGadget
-- instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadGadget s m
--     , Command (t m) ~ Command m
--     , AlternativeIO (t m), Also () (t m)
--     , MonadCommand (t m)
--     ) => MonadGadget s (t m)

instance {-# OVERLAPPABLE #-} (MonadGadget s m) => MonadGadget s (IdentityT m)

instance {-# OVERLAPPABLE #-} (MonadGadget s m) => MonadGadget s (ReaderT r m)

instance {-# OVERLAPPABLE #-} (MonadGadget s m) => MonadGadget s (GadgetT m)

instance {-# OVERLAPPABLE #-} (CmdReactor c, c ~ Command (Widget s c)) => MonadGadget s (Widget s c)
instance {-# OVERLAPPABLE #-} (CmdReactor c, c ~ Command (Gadget s c)) => MonadGadget s (Gadget s c)

-- A 'MonadWidget' is a 'MonadGadget' that additionally have access to
-- 'initConstructor', 'initDestructor', 'initRendered',
-- can generate 'Markup' and so should not be be for event handling, sice those
-- additional effects are ignored inside event handling.
-- 'GadgetT' is *not* an instance of 'MonadWidget'
class (CmdReactor (Command m)
    , MonadGadget s m, PutMarkup m, PutReactPath m
    , AskConstructor m, AskDestructor m, AskRendered m) => MonadWidget s m

instance {-# OVERLAPPABLE #-} (MonadWidget s m) => MonadWidget s (IdentityT m)

instance {-# OVERLAPPABLE #-} (MonadWidget s m) => MonadWidget s (ReaderT r m)

instance {-# OVERLAPPABLE #-} (CmdReactor c, c ~ Command (Widget s c)) => MonadWidget s (Widget s c)

-- | Describes the effects required by 'Widget' to manipulate 'Obj'.
-- 'Reactor' is not a functor because of the @Widget c@ in 'MkObj'
-- which is in a positive agument position.
data Reactor c where

    MkReactId :: (ReactId -> c) -> Reactor c

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

    -- | Make a fully initialized 'Obj' from a widget and model
    -- 'Reactor' is not a functor because of the @Widget@ in 'MkObj'
    -- which is in a positive agument position.
    MkObj :: Widget s c () -> LogName -> (IORef Notifier, Weak (IORef Notifier), MVar s, Weak (MVar s)) -> (Obj s -> c) -> Reactor c

    -- Modifies the model
    Mutate :: Weak (MVar s) -> StateT s IO c -> Reactor c

    -- Notifies any watchers (from 'readWeakObj')
    -- that the model has changed so that the watchers can rerender.
    -- Any rerendering is batched and might be be done immediately
    NotifyDirty :: Weak (IORef Notifier) -> Reactor c

-- instance (IsString str, Semigroup str) => ShowIO str (Reactor c) where
--     showsPrecIO p (MkHandler this _ _ _) = showParenIO (p >= 11) $ (showStr "MkHandler " .) <$> (showsIO this)
--     showsPrecIO p (MkListener this _ _) = showParenIO (p >= 11) $ (showStr "MkListener " .) <$> (showsIO this)
--     showsPrecIO p (MkObj _ logname _ _) = showParenIO (p >= 11) $ (showStr "MkObj " .) <$> (showsIO logname)
--     showsPrecIO p (ReadObj this _ _) = showParenIO (p >= 11) $ (showStr "ReadObj " .) <$> (showsIO this)
--     showsPrecIO p (Mutate this _ req _) = showParenIO (p >= 11) $ (\x -> (showStr "Mutate ") . x . (showFromStr " ") . (showsStr req)) <$> (showsIO this)

mkReactId :: (CmdReactor (Command m), MonadCommand m) => m ReactId
mkReactId = delegatify $ exec' . MkReactId

mkModel :: (MonadIO m, CmdReactor (Command m), MonadCommand m) => s -> m (IORef Notifier, Weak (IORef Notifier), MVar s, Weak (MVar s))
mkModel s = do
    i <- mkReactId
    notifierRef <- liftIO $ newIORef $ Notifier i mempty
    notifierWkRef <- liftIO $ mkWeakIORef notifierRef $ do
        ws <- liftIO $ watchers <$> readIORef notifierRef
        foldMap (unregisterFromNotifier i) ws
    mdlVar <- liftIO $ newMVar s
    mdlWkVar <- liftIO $ mkWeakMVar mdlVar (pure ())
    pure (notifierRef, notifierWkRef, mdlVar, mdlWkVar)
  where
    unregisterFromNotifier :: ReactId -> Weak (IORef Plan) -> IO ()
    unregisterFromNotifier i plnWkRef = (`evalMaybeT` ()) $ do
        plnRef <- fromJustIO $ deRefWeak plnWkRef
        liftIO $ atomicModifyIORef_' plnRef (_notifiers.at i .~ Nothing)


watchModel :: MonadIO m => (IORef Plan, Weak (IORef Plan)) -> (IORef Notifier, Weak (IORef Notifier)) -> m ()
watchModel (plnRef, plnWkRef) (notifierRef, notifierWkRef) = do
    notiId <- liftIO $ notifierId <$> readIORef notifierRef
    watcherId <- liftIO $ planId <$> readIORef plnRef
    liftIO $ atomicModifyIORef_' notifierRef (_watchers.at watcherId .~ Just plnWkRef)
    liftIO $ atomicModifyIORef_' plnRef (_notifiers.at notiId .~ Just notifierWkRef)

unwatchModel :: MonadIO m => IORef Plan -> IORef Notifier -> m ()
unwatchModel plnRef notifierRef = do
    notiId <- liftIO $ notifierId <$> readIORef notifierRef
    watcherId <- liftIO $ planId <$> readIORef plnRef
    liftIO $ atomicModifyIORef_' notifierRef (_watchers.at watcherId .~ Nothing)
    liftIO $ atomicModifyIORef_' plnRef (_notifiers.at notiId .~ Nothing)
