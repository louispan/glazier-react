{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glazier.React.Reactor.Run where

import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.IORef
import Data.Foldable
import Control.Monad.Morph
import qualified GHCJS.Foreign.Callback as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import Glazier.React.Reactor as R
import JavaScript.Extras as JE

newtype IOReactor a = IOReactor
    { runIOReactor :: ReaderT (IORef Int, R.ReactComponent) IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader (IORef Int, R.ReactComponent)
               )

instance MonadReactor IOReactor where
    doNewIORef = liftIO . newIORef
    doReadIORef = liftIO . readIORef
    doWriteIORef v a = liftIO $ writeIORef v a
    mkCallback goStrict goLazy exec = do
        env <- ask
        let goLazy' a = hoist (\m -> runReaderT (runIOReactor m) env) (goLazy a)
            goLazy'' a = goLazy' a >>= traverse_ exec
            f = R.handleEventM goStrict (traverse_ goLazy'')
        liftIO $ J.syncCallback1 J.ContinueAsync (void . runMaybeT . f)
    mkRenderer rnd = do
        env <- ask
        liftIO $ J.syncCallback' (runReaderT (runIOReactor (JE.toJS <$> R.toElement rnd)) env)
    getComponent = do
        (_, c) <- ask
        pure c
    mkKey = do
        (v, _) <- ask
        i <- liftIO $ readIORef v
        liftIO $ modifyIORef' v (\j -> (j `mod` JE.maxSafeInteger) + 1)
        pure i
