{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glazier.React.Reactor.Run where

import Control.Monad.Reader
import Data.IORef
import Data.Function
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
    doModifyIORef' v f = liftIO $ modifyIORef' v f
    mkIO f = do
        env <- ask
        let g = (env &) . runReaderT . runIOReactor . f
        pure g
    mkCallback goStrict goLazy = do
        let f = R.handleEventM goStrict goLazy
        liftIO $ J.syncCallback1 J.ContinueAsync (void . f)
    mkRenderer rnd = do
        env <- ask
        liftIO $ J.syncCallback' ((env &) . runReaderT . runIOReactor $ JE.toJS <$> R.toElement rnd)
    getComponent = do
        (_, c) <- ask
        pure c
    mkKey = do
        (v, _) <- ask
        i <- liftIO $ readIORef v
        liftIO $ modifyIORef' v (\j -> (j `mod` JE.maxSafeInteger) + 1)
        pure i
