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

import Control.Applicative
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Morph
import Control.Monad.Observer
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Function.Extras
import Data.String
import Data.Tagged.Extras
import Glazier.Command

----------------------------------------------------------------

-- | This newtype prevent impredicative polymorphism
newtype ModifyModel s = ModifyModel { unModifyModel :: forall a. MaybeT (State s) a -> IO (Maybe a) }

-- | @IO (Maybe s)@ allows for a 'ZoomModel' instance without exceptions.
type AskModelEnv s m = (MonadAsk "Model" (Maybe s, IO (Maybe s), ModifyModel s) m)
instance {-# OVERLAPPING #-} Monad m => MonadAsk "Model" (Maybe s, IO (Maybe s), ModifyModel s) (ReaderT (Tagged "Model" (Maybe s, IO (Maybe s), ModifyModel s)) m) where
    askEnvP _ = (untag' @"Model") <$> ask
    localEnvP _ f = local (Tagged @"Model" . f . untag' @"Model")

askModelEnv :: AskModelEnv s m => m (Maybe s, IO (Maybe s), ModifyModel s)
askModelEnv = askEnvP @"Model" Proxy

localModelEnv :: AskModelEnv s m => ((Maybe s, IO (Maybe s), ModifyModel s) -> (Maybe s, IO (Maybe s), ModifyModel s)) -> m b -> m b
-- FIXME: javascript compiles with
-- /Users/louis/repo/js/glazier-react-examples/examples/todo/dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/glazier-react-2.0.0.0/build/Glazier/React/Reactor.js_hi
-- Declaration for $w$clocalEnkP2
-- Unfolding of $w$clocalEnkP2:
--   Iface type variable out of scope:  a1
localModelEnv f = fixme $ localEnvP @"Model" Proxy f

---------------------------------------------------------------------------

-- | Something that has access to a model, to ask an initial value, or read the latest value
-- or write the latest value
class (AlternativeIO m, AskModelEnv s m) => MonadModel s m

-- | Any transformer on top of 'MonadModel' is also a 'MonadModel'
instance {-# OVERLAPPABLE #-} (Alternative (t m), MonadIO (t m), Monad (t m), MonadTrans t, MFunctor t, MonadModel s m) => MonadModel s (t m)

instance (AlternativeIO m) => MonadModel s (ModelT s m)

---------------------------------------------------------------------------

type ModelT' s m = ReaderT (Tagged "Model" (Maybe s, IO (Maybe s), ModifyModel s)) m

-- | 'ModelT' is transformer that add the 'MonadModel' instance.
newtype ModelT s m a = ModelT { unModelT :: ModelT' s m a }
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
    , MonadAsk "Model" (Maybe s, IO (Maybe s), ModifyModel s) -- AskModelEnv
    )

type instance Command (ModelT s m) = Command m

instance MonadTrans (ModelT s) where
    lift = ModelT . lift

instance MFunctor (ModelT s) where
    hoist nat (ModelT m) = ModelT (hoist nat m)

deriving via (IdentityT (ModelT' s m)) instance MonadAsk p r m => MonadAsk p r (ModelT s m)
deriving via (IdentityT (ModelT' s m)) instance MonadPut p t m => MonadPut p t (ModelT s m)

-- | This instance of MonadCodify makes sure that the cached state
-- the updated with the latest state from the ref.
-- This also has the effect of allowing GHC to make the same 'StableName'
-- for a codified monad irregardless of the value of cached state.
instance (MonadIO m, MonadCodify m) => MonadCodify (ModelT s m) where
        codify f = do
            (_, y, z) <- askModelEnv
            x <- liftIO y
            lift $ codify $ \a -> (`runModelT` (x, y, z)) $ f a

instance (Monad m, MonadObserver p a m) => MonadObserver p a (ModelT s m) where
    askObserver p = lift $ (lift .) <$> askObserver p

runModelT :: ModelT s m a -> (Maybe s, IO (Maybe s), ModifyModel s) -> m a
runModelT (ModelT m) t = runReaderT m $ Tagged @"Model" t

fromModelT :: MonadModel s m => ModelT s m a -> m a
fromModelT m = askModelEnv >>= runModelT m

---------------------------------------------------------------------------

class (MonadModel s m, MonadModel t n) => ZoomModel m n s t | m -> s, n -> t, m t -> n, n s -> m where
    zoomModel :: Traversal' t s -> m c -> n c

instance AlternativeIO m => ZoomModel (ModelT s m) (ModelT t m) s t where
    zoomModel l (ModelT m) = ModelT $ ReaderT $ \(Tagged (x, y, z)) ->
        let x' = x >>= r
            y' = (>>= r) <$> y
            -- z :: (MaybeT (State t) a -> IO (Maybe a)
            -- z' :: (MaybeT (State s) a -> IO (Maybe a)
            -- f :: MaybeT (State s) a
            -- z' zz f = (unModelEnv zz) (g f)
            -- g :: MaybeT (State s) a -> MaybeT (State t) a

        in runReaderT m $ Tagged @"Model" (x', y', h z)
      where
        -- r :: t -> Maybe s
        r = preview l
        -- w :: s -> t -> t
        w = set l
        h (ModifyModel g) = ModifyModel $ \f ->
            g $ do
                t <- get
                case r t of
                    Nothing -> empty
                    Just s -> case runState (runMaybeT f) s of
                        (Nothing, _) -> empty
                        (Just a, s') -> do
                            put $ w s' t
                            pure a

-- | This instance allows using "plain string" in 'txt', and in props for 'lf', and 'bh'
-- when using @OverloadedString@ with @ExtendedDefaultRules@
instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (ModelT s m a) where
    fromString = pure . fromString

-- instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (GadgetT m (Maybe a)) where
--     fromString = pure . Just . fromString
