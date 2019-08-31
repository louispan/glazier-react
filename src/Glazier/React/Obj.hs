{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Obj where

-- import Data.Diverse.Lens
import Control.Monad.Benign
import Control.Monad.Context
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.IORef
import Glazier.Logger
import Glazier.React.Model
import System.Mem.Weak

type Obj s = IORef (Model s)
type WeakObj s = Weak (IORef (Model s))

-- benignReadWeakObjModel :: MonadBenignIO m => WeakObj s -> m (Maybe (Model s))
-- benignReadWeakObjModel obj = runMaybeT $ do
--     mdlRef <- MaybeT . liftBenignIO . benignDeRefWeak $ sceneWeakRef obj
--     liftBenignIO $ benignReadIORef mdlRef

-----------------------------------------------

type AskWeakObj s = MonadAsk (WeakObj s)
askWeakObj :: AskWeakObj s m => m (WeakObj s)
askWeakObj = askContext'

instance Monad m => MonadAsk (Benign IO (Maybe LogLevel)) (ReaderT (WeakObj s) m) where
    -- logLevel :: m (Benign IO (Maybe LogLevel))
    askContext = do
        obj <- askWeakObj
        pure $ Benign $ go obj
      where
        go wk = runMaybeT $ do
            ref <- maybeM $ deRefWeak wk
            s <- lift $ readIORef ref
            maybeM . planLogLevel . plan $ s
