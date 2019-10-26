{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Gadget.Internal where

import Control.Also
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Morph
import Data.String
import Glazier.Command

-- | A newtype wrapper to indicate that only 'Glazier.React.Reactant.MonadGadget'
-- effect are allowed.
-- 'GadgetT' is an instance of 'Glazier.React.Reactant.MonadGadget'
-- 'GadgetT' is *not* an instance of 'Glazier.React.Reactant.MonadWidget'
type instance Command (GadgetT m) = Command m

newtype GadgetT m a = GadgetT { runGadgetT :: m a}
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
    , MonadAsk p r
    )

instance MonadTrans GadgetT where
    lift = GadgetT

instance MFunctor GadgetT where
    hoist nat m = GadgetT (nat (runGadgetT m))

-- | This instance allows using "plain string" in 'txt', and in props for 'lf', and 'bh'
-- when using @OverloadedString@ with @ExtendedDefaultRules@
instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (GadgetT m a) where
    fromString = pure . fromString

instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (GadgetT m (Maybe a)) where
    fromString = pure . Just . fromString
