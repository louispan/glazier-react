
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

import Control.Also
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Observer
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import Data.String
import Data.Tagged.Extras
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Plan
import Glazier.React.ReactId
import Glazier.React.ReactPath
import qualified JavaScript.Extras as JE
import System.Mem.Weak

type AskScratch = MonadAsk' (Tagged "Scratch" JE.Object)
instance {-# OVERLAPPING #-} Monad m => MonadAsk (Tagged "Scratch" JE.Object) (Tagged "Scratch" JE.Object) (ReaderT (Tagged "Scratch" JE.Object) m) where
    askEnviron _ = ask
    localEnviron _ = local

askScratch :: AskScratch m => m JE.Object
askScratch = (untag' @"Scratch") <$> askEnviron @(Tagged "Scratch" JE.Object) Proxy
localScratch :: AskScratch m => (JE.Object -> JE.Object) -> m a -> m a
localScratch f = localEnviron @(Tagged "Scratch" JE.Object) Proxy (Tagged @"Scratch" . f . untag' @"Scratch")

scratchAccessor :: ReactId -> J.JSString -> J.JSString
scratchAccessor i n = "$" <> (fromString $ show $ unReactId i) <> "_" <> n

deleteScratch :: (MonadIO m, AskScratch m) => ReactId -> J.JSString -> m ()
deleteScratch i n = do
    d <- askScratch
    liftIO $ JE.deleteProperty d (scratchAccessor i n)

setScratch :: (MonadIO m, AskScratch m, JE.ToJS a) => ReactId -> J.JSString -> a -> m ()
setScratch i n v = do
    d <- askScratch
    liftIO $ JE.setProperty d (scratchAccessor i n) (JE.toJS v)

getScratch :: (MonadIO m, AskScratch m) => ReactId -> J.JSString -> m J.JSVal
getScratch i n = do
    d <- askScratch
    liftIO $ JE.getProperty d (scratchAccessor i n)

scratchXTimes :: (MonadIO m, AskScratch m) => Int -> ReactId -> J.JSString -> m () -> m ()
scratchXTimes maxTimes i n m = do
    d <- JE.fromJS @Int <$> getScratch i n
    let (x', m') = case (d, maxTimes) of
            (_, x) | x <= 0         -> (0, pure ())
            (Nothing, _)            -> (1, m)
            (Just y, x) | y < x     -> (y + 1, m)
            _                       -> (maxTimes, pure ())
    setScratch i n x'
    m'

type AskConstructor m = MonadObserver' (Tagged "Constructor" (Command m)) m
askConstructor :: forall m. AskConstructor m => m (Command m -> m ())
askConstructor = (. Tagged @"Constructor") <$> askObserver @(Tagged "Constructor" (Command m)) Proxy

type AskDestructor m = MonadObserver' (Tagged "Destructor" (Command m)) m
askDestructor :: forall m. AskDestructor m => m (Command m -> m ())
askDestructor = (. Tagged @"Destructor") <$> askObserver @(Tagged "Destructor" (Command m)) Proxy

type AskRendered m = MonadObserver' (Tagged "Rendered" (Command m)) m
askRendered :: forall m. AskRendered m => m (Command m -> m ())
askRendered = (. Tagged @"Rendered") <$> askObserver @(Tagged "Rendered" (Command m)) Proxy

-- | Register and execute the given monad at construction time.
-- At construction, 'initDestructor' and 'initRendered' may be used.
-- The given monad is only performed at construction of the widget.
-- That is, on subsequent rerendesrs, @initConstructor = const $ pure ()@
-- Do not expect this function to do anything on subsequent rerenders
-- so don't use the function conditionally or inside event handling code.
initConstructor :: forall m. (AskConstructor m, MonadCodify m) => m () -> m ()
initConstructor m = do
    f <- askConstructor
    c <- codify' m
    f c

-- | Register the given monad to be evaluated at destruction time.
-- The same "construction time only registration caveats" apply as in 'initConstructor'.
initDestructor :: forall m. (AskDestructor m, MonadCodify m) => m () -> m ()
initDestructor m = do
    f <- askDestructor
    c <- codify' m
    f c

-- | Register the given monad to be evaluated after every rerender, including the first rerender.
-- The same "construction time only registration caveats" apply as in 'initConstructor'.
initRendered :: forall m. (AskRendered m, MonadCodify m) => m () -> m ()
initRendered m = do
    f <- askRendered
    c <- codify' m
    f c

type Reactor' c =
    ObserverT (Tagged "Rendered" c) -- 'AskRendered'
    (ObserverT (Tagged "Destructor" c) -- 'AskDestructor'
    (ObserverT (Tagged "Constructor" c) -- 'AskConstructor'
    (ReaderT (Tagged "Scratch" JE.Object) -- 'AskScratch'
    (ReaderT (Weak (IORef Notifier)) -- 'AskNotifierWeakRef'
    (ReaderT (Weak (IORef Plan)) -- 'AskPlanWeakRef', 'AskLogLevel', 'AskLogCallStackDepth', 'AskLogName'
    (MaybeT -- 'Alternative'
    (ContT () -- 'MonadDelegate'
    -- State monads must be inside ContT to be a 'MonadDelegate'
    (StateT ReactPath -- 'PutReactPath', 'AskReactPath'
    (StateT (DL.DList ReactMarkup) -- 'PutMarkup'
    (ProgramT c IO -- 'MonadComand', 'MonadIO'
    ))))))))))

type instance Command (Reactor c) = c

-- | Reactor is a concreate transformer stack that is an instance of 'Glazier.React.Rector.Internal.MonadGadget'
-- A newtype is required for the instance of 'Glazier.React.Rector.Internal.MonadGadget'
newtype Reactor c a = Reactor { runReactor :: Reactor' c a }
    deriving
    ( Functor
    , Applicative
    , Also ()
    , Monad
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadCont
    , MonadDelegate
    , MonadProgram
    , AskMarkup
    , PutMarkup
    , AskReactPath
    , PutReactPath
    , AskLogLevel
    , AskLogName
    , AskLogCallStackDepth
    , AskPlanWeakRef
    , AskNotifierWeakRef
    , AskScratch
    )

deriving via (IdentityT (Reactor' c)) instance (Cmd' IO c, Cmd' [] c) => MonadCodify (Reactor c)
deriving via (IdentityT (Reactor' c)) instance MonadObserver (Tagged "Constructor" c) (Tagged "Constructor" c) (Reactor c)
deriving via (IdentityT (Reactor' c)) instance MonadObserver (Tagged "Destructor" c) (Tagged "Destructor" c) (Reactor c)
deriving via (IdentityT (Reactor' c)) instance MonadObserver (Tagged "Rendered" c) (Tagged "Rendered" c) (Reactor c)

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

