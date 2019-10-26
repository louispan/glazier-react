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

module Glazier.React.Model where

import Control.Also
import Control.Applicative
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Trans.Identity
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.Tagged.Extras
import Glazier.Command
import System.Mem.Weak

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

-- | Something that has access to a model from the environment, and also
-- a ref to apply mutations to that model
class (AskModelWeakVar s m, AskModel s m) => MonadModel s m

-- | Any transformer on top of 'MonadModel' is also a 'MonadModel'
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MFunctor t, MonadModel s m) => MonadModel s (t m)

instance Monad m => MonadModel s (ModelT s m)

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

fromModelT :: MonadModel s m => ModelT s m a -> m a
fromModelT m = do
    mdlWkVar <- askModelWeakVar
    mdl <- askModel
    runModelT m (mdlWkVar, mdl)

type instance Command (ModelT s m) = Command m

instance MonadTrans (ModelT s) where
    lift = ModelT . lift . lift

instance MFunctor (ModelT s) where
    hoist nat (ModelT m) = ModelT (hoist (hoist nat) m)

deriving via (IdentityT (ModelT' s m)) instance MonadAsk p r m => MonadAsk p r (ModelT s m)
deriving via (IdentityT (ModelT' s m)) instance MonadPut p t m => MonadPut p t (ModelT s m)

