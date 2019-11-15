
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.ACont
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import Data.Tagged.Extras
import Glazier.Command
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Plan
import Glazier.React.ReactPath
import JS.Data
import System.Mem.Weak

type AskScratch = MonadAsk' (Tagged "Scratch" JSObject)
askScratch :: AskScratch m => m JSObject
askScratch = askTagged @"Scratch" @JSObject
localScratch :: AskScratch m => (JSObject -> JSObject) -> m a -> m a
localScratch = localTagged @"Scratch" @JSObject

deleteScratch :: (MonadIO m, AskScratch m) => JSString -> m ()
deleteScratch n = do
    d <- askScratch
    liftIO $ d `deleteProperty` n

setScratch :: (MonadIO m, AskScratch m, ToJS a) => JSString -> a -> m ()
setScratch n v = do
    d <- askScratch
    liftIO $ d `setProperty` (n, toJS v)

getScratch :: (MonadIO m, AskScratch m) => JSString -> m JSVal
getScratch n = do
    d <- askScratch
    liftIO $ d `getProperty` n

scratchTimes :: (MonadIO m, AskScratch m) => Int -> JSString -> m () -> m ()
scratchTimes maxTimes n m = do
    d <- fromJS @Int <$> getScratch n
    let (x', m') = case (d, maxTimes) of
            (_, x) | x <= 0         -> (0, pure ())
            (Nothing, _)            -> (1, m)
            (Just y, x) | y < x     -> (y + 1, m)
            _                       -> (maxTimes, pure ())
    setScratch n x'
    m'

type Reactor' c =
    ReaderT (Tagged "Scratch" JSObject) -- 'AskScratch'
    (ReaderT (Weak (IORef Notifier)) -- 'AskNotifierWeakRef'
    (ReaderT (Weak (IORef Plan)) -- 'AskPlanWeakRef', 'AskLogLevel', 'AskLogCallStackDepth', 'AskLogName'
    (AContT () -- 'MonadDelegate', 'MonadDischarge'
    (MaybeT -- 'Alternative'
    -- State monads must be inside ContT to be a 'MonadDelegate'
    (StateT ReactPath -- 'PutReactPath', 'AskReactPath'
    (StateT (DL.DList ReactMarkup) -- 'PutMarkup'
    (ProgramT c IO -- 'MonadComand', 'MonadIO'
    )))))))

type instance Command (Reactor c) = c

-- | Reactor is a concreate transformer stack that is an instance of 'Glazier.React.Rector.Internal.MonadGadget'
-- A newtype is required for the instance of 'Glazier.React.Rector.Internal.MonadGadget'
newtype Reactor c a = Reactor { runReactor :: Reactor' c a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadCont
    , MonadDelegate
    , MonadDischarge
    , MonadProgram
    , MonadAsk' Markup
    , MonadPut' Markup
    , AskLogLevel
    , AskLogCallStackDepth
    , MonadAsk' LogName
    , MonadAsk' ReactPath
    , MonadPut' ReactPath
    , AskPlanWeakRef
    , AskNotifierWeakRef
    , AskScratch
    )

deriving via (IdentityT (Reactor' c)) instance (Cmd' IO c, Cmd' [] c) => MonadCodify (Reactor c)

-- | 'Widget' is a concrete transformer stack that is an instance of 'MonadModel'
-- 'Glazier.React.Rector.Internal.MonadGadget' and 'Glazier.React.Rector.Internal.MonadWidget'
type Widget s c = ModelT s (Reactor c)

-- | ALlow additional user ReaderT and IdentityT stack on top of @Widget s@
-- Like 'Control.Monad.IO.Unlift.UnliftIO', this newtype wrapper prevents impredicative types.
newtype UnliftWidget s m = UnliftWidget { unliftWidget :: forall a. m a -> Widget s (Command m) a }

-- | Similar to 'Control.Monad.IO.Unlift.MonadUnliftIO', except we want to unlift a @Widget s m a@.
-- This limits transformers stack to 'ReaderT' and 'IdentityT' on top of @Widget s m@
-- Example
-- @
-- unliftMkObj :: (MonadUnliftWidget s m, MonadGadget s m)
--     => m () -> LogName -> s -> m (Obj s)
-- unliftMkObj m logname s = do
--     u <- askUnliftWidget
--     mkObj (unliftWidget u m) logname s
-- @
class MonadUnliftWidget s m | m -> s where
    askUnliftWidget :: m (UnliftWidget s m)

instance MonadUnliftWidget s (Widget s c) where
    askUnliftWidget = pure (UnliftWidget id)

instance (Functor m, MonadUnliftWidget s m) => MonadUnliftWidget s (ReaderT r m) where
    askUnliftWidget = ReaderT $ \r ->
        (\u -> UnliftWidget (unliftWidget u . flip runReaderT r)) <$> askUnliftWidget

instance (Functor m, MonadUnliftWidget s m) => MonadUnliftWidget s (IdentityT m) where
    askUnliftWidget = IdentityT $
        (\u -> UnliftWidget (unliftWidget u . runIdentityT)) <$> askUnliftWidget
