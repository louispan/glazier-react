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
{-# LANGUAGE ViewPatterns #-}

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
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Identity
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.Tagged.Extras
import Glazier.Command
import System.Mem.Weak

----------------------------------------------------------------

type AskModelWeakVar s = MonadAsk "ModelWeakVar" (Tagged "ModelWeakVar" (Weak (MVar s)))
instance {-# OVERLAPPING #-} Monad m => MonadAsk "ModelWeakVar" (Tagged "ModelWeakVar" (Weak (MVar s))) (ReaderT (Tagged "ModelWeakVar" (Weak (MVar s))) m) where
    askEnviron _ = ask
    localEnviron _ = local

askModelWeakVar :: AskModelWeakVar s m => m (Weak (MVar s))
askModelWeakVar = (untag' @"ModelWeakVar") <$> askEnviron @"ModelWeakVar" Proxy
localModelWeakVar :: AskModelWeakVar s m => (Weak (MVar s) -> Weak (MVar s)) -> m a -> m a
localModelWeakVar f = localEnviron @"ModelWeakVar" Proxy (Tagged @"ModelWeakVar" . f . untag' @"ModelWeakVar")

----------------------------------------------------------------

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

----------------------------------------------------------------

type AskModel2 s m = (AlternativeIO m, MonadAsk "Model2" (Maybe s, IO (Maybe s), (s -> s) -> IO ()) m)
instance {-# OVERLAPPING #-} Monad m => MonadAsk "Model2" (Maybe s, IO (Maybe s), (s -> s) -> IO ()) (ReaderT (Tagged "Model2" (Maybe s, IO (Maybe s), (s -> s) -> IO ())) m) where
    askEnviron _ = (untag' @"Model2") <$> ask
    localEnviron _ f = local (Tagged @"Model2" . f . untag' @"Model2")

askModel2 :: AskModel2 s m => m (Maybe s, IO (Maybe s), (s -> s) -> IO ())
askModel2 = askEnviron @"Model2" Proxy

localModel2 :: AskModel2 s m => ((Maybe s, IO (Maybe s), (s -> s) -> IO ()) -> (Maybe s, IO (Maybe s), (s -> s) -> IO ())) -> m a -> m a
localModel2 f = localEnviron @"Model2" Proxy f

-- | This is like 'view' but for the cached value in 'AskModel2', not 'MonadReader'
model2 :: AskModel2 s m => Getting a s a -> m a
model2 l = do
    (ms, _, _) <- askModel2
    s <- guardJust ms
    pure (getConst #. l Const $ s)

-- | This is like 'preview' but for the cached value in  'AskModel2', not 'MonadReader'
premodel2 :: AskModel2 s m => Getting (First a) s a -> m (Maybe a)
premodel2 l = do
    (ms, _, _) <- askModel2
    s <- guardJust ms
    pure (getFirst #. foldMapOf l (First #. Just) $ s)

----------------------------------------------------------------

-- | This is like 'view' but for the @IO (Maybe s)@ in 'AskModel2', not 'MonadReader'
readModel2 :: (MonadIO m, AskModel2 s m) => Getting a s a -> m a
readModel2 l = do
    (_, ms, _) <- askModel2
    s <- guardJustIO ms
    pure (getConst #. l Const $ s)

-- | This is like 'preview' but for the @IO (Maybe s)@ in 'AskModel2', not 'MonadReader'
prereadModel2 :: (MonadIO m, AskModel2 s m) => Getting (First a) s a -> m (Maybe a)
prereadModel2 l = do
    (_, ms, _) <- askModel2
    s <- guardJustIO ms
    pure (getFirst #. foldMapOf l (First #. Just) $ s)

----------------------------------------------------------------

writeModel :: (MonadIO m, AskModel2 s m) => Setter' s a -> a -> m ()
writeModel l a = do
    (_, _, f) <- askModel2
    liftIO . f $ set l a

overModel :: (MonadIO m, AskModel2 s m) => Setter' s a -> (a -> a) -> m ()
overModel l g = do
    (_, _, f) <- askModel2
    liftIO . f $ over l g

----------------------------------------------------------------

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

type instance Command (ModelT s m) = Command m

instance MonadTrans (ModelT s) where
    lift = ModelT . lift . lift

instance MFunctor (ModelT s) where
    hoist nat (ModelT m) = ModelT (hoist (hoist nat) m)

deriving via (IdentityT (ModelT' s m)) instance MonadAsk p r m => MonadAsk p r (ModelT s m)
deriving via (IdentityT (ModelT' s m)) instance MonadPut p t m => MonadPut p t (ModelT s m)

runModelT :: ModelT s m a -> (Weak (MVar s), s) -> m a
runModelT (ModelT m) (mdlWkVar, mdl) = (`runReaderT` (Tagged @"Model" mdl))
    . (`runReaderT` (Tagged @"ModelWeakVar" mdlWkVar)) $ m

fromModelT :: MonadModel s m => ModelT s m a -> m a
fromModelT m = do
    mdlWkVar <- askModelWeakVar
    mdl <- askModel
    runModelT m (mdlWkVar, mdl)





-- | Something that has access to a model, to ask an initial value, or read the latest value
-- or write the latest value
class (AskModel2 s m) => MonadModel2 s m

-- | Any transformer on top of 'MonadModel' is also a 'MonadModel'
instance {-# OVERLAPPABLE #-} (Alternative (t m), MonadIO (t m), Monad (t m), MonadTrans t, MFunctor t, MonadModel2 s m) => MonadModel2 s (t m)

instance (AlternativeIO m) => MonadModel2 s (ModelT2 s m)


class (MonadModel2 s m, MonadModel2 t n) => ZoomModel m n s t | m -> s, n -> t, m t -> n, n s -> m where
    zoomModel :: Traversal' t s -> m c -> n c

instance AlternativeIO m => ZoomModel (ModelT2 s m) (ModelT2 t m) s t where
    zoomModel l (ModelT2 m) = ModelT2 $ ReaderT $ \(untag' @"Model2" -> (x, y, z)) ->
        let x' = x >>= r
            y' = (>>= r) <$> y
            -- z :: (t -> t) -> IO ()
            -- z' :: (s -> s) -> IO ()
            -- f :: (s -> s)
            z' f = z (g f)
            -- g :: (s -> s) -> t -> t
            g f t = case r t of
                        Nothing -> t -- will still cause a write to happen :(
                        Just s -> w (f s) t
        in runReaderT m $ Tagged @"Model2" (x', y', z')
      where
        r = preview l
        w = set l

type ModelT2' s m = ReaderT (Tagged "Model2" (Maybe s, IO (Maybe s), (s -> s) -> IO ())) m

-- | 'ModelT' is transformer that add the 'MonadModel' instance.
newtype ModelT2 s m a = ModelT2 { unModelT2 :: ModelT2' s m a }
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
    , MonadAsk "Model2" (Maybe s, IO (Maybe s), (s -> s) -> IO ()) -- AskModel2
    )

type instance Command (ModelT2 s m) = Command m

instance MonadTrans (ModelT2 s) where
    lift = ModelT2 . lift

instance MFunctor (ModelT2 s) where
    hoist nat (ModelT2 m) = ModelT2 (hoist nat m)
deriving via (IdentityT (ModelT2' s m)) instance MonadAsk p r m => MonadAsk p r (ModelT2 s m)
deriving via (IdentityT (ModelT2' s m)) instance MonadPut p t m => MonadPut p t (ModelT2 s m)

runModelT2 :: ModelT2 s m a -> (Maybe s, IO (Maybe s), (s -> s) -> IO ()) -> m a
runModelT2 (ModelT2 m) t = runReaderT m $ Tagged @"Model2" t

fromModelT2 :: MonadModel2 s m => ModelT2 s m a -> m a
fromModelT2 m = askModel2 >>= runModelT2 m



