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
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Tagged.Extras
import Glazier.Command

----------------------------------------------------------------

-- | This newtype prevent impredicative polymorphism
newtype ModelState s = ModelState { unModelState :: forall a. MaybeT (State s) a -> IO (Maybe a) }

type AskModel s m = (MonadAsk "Model" (Maybe s, IO (Maybe s), ModelState s) m)
instance {-# OVERLAPPING #-} Monad m => MonadAsk "Model" (Maybe s, IO (Maybe s), ModelState s) (ReaderT (Tagged "Model" (Maybe s, IO (Maybe s), ModelState s)) m) where
    askEnviron _ = (untag' @"Model") <$> ask
    localEnviron _ f = local (Tagged @"Model" . f . untag' @"Model")

askModel :: AskModel s m => m (Maybe s, IO (Maybe s), ModelState s)
askModel = askEnviron @"Model" Proxy

localModel :: AskModel s m => ((Maybe s, IO (Maybe s), ModelState s) -> (Maybe s, IO (Maybe s), ModelState s)) -> m b -> m b
localModel f = localEnviron @"Model" Proxy f

-- | Something that has access to a model, to ask an initial value, or read the latest value
-- or write the latest value
class (AlternativeIO m, AskModel s m) => MonadModel s m

-- | Any transformer on top of 'MonadModel' is also a 'MonadModel'
instance {-# OVERLAPPABLE #-} (Alternative (t m), MonadIO (t m), Monad (t m), MonadTrans t, MFunctor t, MonadModel s m) => MonadModel s (t m)

instance (AlternativeIO m) => MonadModel s (ModelT s m)

class (MonadModel s m, MonadModel t n) => ZoomModel m n s t | m -> s, n -> t, m t -> n, n s -> m where
    zoomModel :: Traversal' t s -> m c -> n c

instance AlternativeIO m => ZoomModel (ModelT s m) (ModelT t m) s t where
    zoomModel l (ModelT m) = ModelT $ ReaderT $ \(Tagged (x, y, z)) ->
        let x' = x >>= r
            y' = (>>= r) <$> y
            -- z :: (MaybeT (State t) a -> IO (Maybe a)
            -- z' :: (MaybeT (State s) a -> IO (Maybe a)
            -- f :: MaybeT (State s) a
            -- z' zz f = (unModelState zz) (g f)
            -- g :: MaybeT (State s) a -> MaybeT (State t) a

        in runReaderT m $ Tagged @"Model" (x', y', h z)
      where
        -- r :: t -> Maybe s
        r = preview l
        -- w :: s -> t -> t
        w = set l
        h (ModelState g) = ModelState $ \f ->
            g $ do
                t <- get
                case r t of
                    Nothing -> empty
                    Just s -> case runState (runMaybeT f) s of
                        (Nothing, _) -> empty
                        (Just a, s') -> do
                            put $ w s' t
                            pure a


type ModelT' s m = ReaderT (Tagged "Model" (Maybe s, IO (Maybe s), ModelState s)) m

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
    , MonadAsk "Model" (Maybe s, IO (Maybe s), ModelState s) -- AskModel
    )

type instance Command (ModelT s m) = Command m

instance MonadTrans (ModelT s) where
    lift = ModelT . lift

instance MFunctor (ModelT s) where
    hoist nat (ModelT m) = ModelT (hoist nat m)
deriving via (IdentityT (ModelT' s m)) instance MonadAsk p r m => MonadAsk p r (ModelT s m)
deriving via (IdentityT (ModelT' s m)) instance MonadPut p t m => MonadPut p t (ModelT s m)

runModelT :: ModelT s m a -> (Maybe s, IO (Maybe s), ModelState s) -> m a
runModelT (ModelT m) t = runReaderT m $ Tagged @"Model" t

fromModelT :: MonadModel s m => ModelT s m a -> m a
fromModelT m = askModel >>= runModelT m



