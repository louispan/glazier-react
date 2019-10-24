{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Glazier.React.Gadget.Internal where

import Control.Also
import Control.Applicative
import Control.Monad.Delegate
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Trans.Identity
import Data.String
import Glazier.Command

-- | A newtype wrapper to indicate that only 'Glazier.React.Reactor.Internal.MonadGadget'
-- effect are allowed.
-- 'GadgetT' is an instance of 'Glazier.React.Reactor.Internal.MonadGadget'
-- 'GadgetT' is *not* an instance of 'Glazier.React.Reactor.Internal.MonadWidget'
newtype GadgetT f a = GadgetT { unGadgetT :: f a}
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , Alternative
    , MonadDelegate
    , MonadCodify
    , MonadProgram
    )

deriving via (IdentityT f) instance (Also a f) => Also a (GadgetT f)

instance MonadTrans GadgetT where
    lift = GadgetT

instance MFunctor GadgetT where
    hoist nat m = GadgetT (nat (unGadgetT m))

-- | This instance allows using "plain string" in 'txt', and in props for 'lf', and 'bh'
-- when using @OverloadedString@ with @ExtendedDefaultRules@
instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (GadgetT m a) where
    fromString = pure . fromString

instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (GadgetT m (Maybe a)) where
    fromString = pure . Just . fromString
