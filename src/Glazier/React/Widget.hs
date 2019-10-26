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

module Glazier.React.Widget where

import Control.Also
import Control.Applicative
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Morph
import Control.Monad.Observer
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.String
import Data.Tagged.Extras
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.Markup
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

type AskModelWeakVar s = MonadAsk "ModelWeakVar" (Tagged "ModelWeakVar" (Weak (MVar s)))
instance {-# OVERLAPPING #-} Monad m => MonadAsk "ModelWeakVar" (Tagged "ModelWeakVar" (Weak (MVar s))) (ReaderT (Tagged "ModelWeakVar" (Weak (MVar s))) m) where
    askEnviron _ = ask
    localEnviron _ = local

askModelWeakVar :: AskModelWeakVar s m => m (Weak (MVar s))
askModelWeakVar = (untag' @"ModelWeakVar") <$> askEnviron @"ModelWeakVar" Proxy
localModelWeakVar :: AskModelWeakVar s m => (Weak (MVar s) -> Weak (MVar s)) -> m a -> m a
localModelWeakVar f = localEnviron @"ModelWeakVar" Proxy (Tagged @"ModelWeakVar" . f . untag' @"ModelWeakVar")

type AskModel s = MonadAsk "Model" (Tagged "Model" s)
instance {-# OVERLAPPING #-} Monad m => MonadAsk "Model" (Tagged "Model" s) (ReaderT (Tagged "Model" s) m) where
    askEnviron _ = ask
    localEnviron _ = local

askModel :: AskModel s m => m s
askModel = (untag' @"Model") <$> askEnviron @"Model" Proxy
localModel :: AskModel s m => (s -> s) -> m a -> m a
localModel f = localEnviron @"Model" Proxy (Tagged @"Model" . f . untag' @"Model")

-- | This is like 'view' but for the 'AskModel', not 'MonadReader'
model :: AskModel s m => Getting a s a -> m a
model l = (getConst #. l Const) <$> askModel

-- | This is like 'preview' but for the 'AskModel', not 'MonadReader'
premodel :: AskModel s m => Getting (First a) s a -> m (Maybe a)
premodel l = (getFirst #. foldMapOf l (First #. Just)) <$> askModel

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

-- | Something that has access to a model from the environment, and also
-- a ref to apply mutations to that model
type MonadModel s m = (AskModelWeakVar s m, AskModel s m)

type ModelT' s m = ReaderT (Tagged "ModelWeakVar" (Weak (MVar s))) -- 'AskModelWeakVar'
        (ReaderT (Tagged "Model" s) m) -- 'AskModel'

-- | 'ModelT' is transformer that add the 'MonadModel' instance.
newtype ModelT s m a = ModelT { unModelT :: ModelT' s m a }
    deriving
    ( Functor
    , Applicative
    , Also r
    , Monad
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadCont
    , MonadDelegate
    , MonadProgram
    , MonadCodify
    , MonadAsk "ModelWeakVar" (Tagged "ModelWeakVar" (Weak (MVar s)))
    , MonadAsk "Model" (Tagged "Model" s)
    )

runModelT :: ModelT s m a -> (Weak (MVar s), s) -> m a
runModelT (ModelT m) (mdlWkVar, mdl) = (`runReaderT` (Tagged @"Model" mdl))
    . (`runReaderT` (Tagged @"ModelWeakVar" mdlWkVar)) $ m

type instance Command (ModelT s m) = Command m

instance MonadTrans (ModelT s) where
    lift = ModelT . lift . lift

instance MFunctor (ModelT s) where
    hoist nat (ModelT m) = ModelT (hoist (hoist nat) m)

deriving via (IdentityT (ModelT' s m)) instance MonadAsk p r m => MonadAsk p r (ModelT s m)
deriving via (IdentityT (ModelT' s m)) instance MonadPut p t m => MonadPut p t (ModelT s m)

type Gizmo' c =
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

type instance Command (Gizmo c) = c

-- | Gizmo is a concreate transformer stack that is an instance of 'Glazier.React.Rector.Internal.MonadGadget'
-- A newtype is required for the instance of 'Glazier.React.Rector.Internal.MonadGadget'
newtype Gizmo c a = Gizmo { runGizmo :: Gizmo' c a }
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

deriving via (IdentityT (Gizmo' c)) instance (Cmd' IO c, Cmd' [] c) => MonadCodify (Gizmo c)
deriving via (IdentityT (Gizmo' c)) instance MonadObserver (Tagged "Constructor" c) (Tagged "Constructor" c) (Gizmo c)
deriving via (IdentityT (Gizmo' c)) instance MonadObserver (Tagged "Destructor" c) (Tagged "Destructor" c) (Gizmo c)
deriving via (IdentityT (Gizmo' c)) instance MonadObserver (Tagged "Rendered" c) (Tagged "Rendered" c) (Gizmo c)

-- | 'Widget' is a concrete transformer stack that is an instance of 'MonadModel'
-- 'Glazier.React.Rector.Internal.MonadGadget' and 'Glazier.React.Rector.Internal.MonadWidget'
type Widget s c = ModelT s (Gizmo c)

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

