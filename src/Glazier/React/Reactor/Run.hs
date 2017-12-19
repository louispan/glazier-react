{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glazier.React.Reactor.Run (
    IOReactor(..)
    ) where

import Control.Monad.Reader
import Data.Coerce
import Data.IORef
import Data.Function
import qualified GHCJS.Foreign.Callback as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import Glazier.React.Reactor as R
import JavaScript.Extras as JE

newtype IOReactor x a = IOReactor
    { runIOReactor :: ReaderT (IORef Int, R.ReactComponent, x -> IO ()) IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader (IORef Int, R.ReactComponent, x -> IO ())
               )

instance MonadReactor x (IOReactor x) where
    doNewIORef = liftIO . newIORef
    doReadIORef = liftIO . readIORef
    doWriteIORef v a = liftIO $ writeIORef v a
    doModifyIORef' v f = liftIO $ modifyIORef' v f
    mkCallback goStrict goLazy = do
        env@(_, _, ex) <- ask
        let goLazy' = (env &) . runReaderT . runIOReactor . goLazy
        let f = R.handleEventM goStrict (goLazy' >=> ex)
        liftIO $ J.syncCallback1 J.ContinueAsync (void . f)
    mkRenderer rnd = do
        env <- ask
        liftIO . coerce $ J.syncCallback' ((env &) . runReaderT . runIOReactor $ JE.toJS <$> R.toElement rnd)
    getComponent = do
        (_, c, _) <- ask
        pure c
    mkSeq = do
        (v, _, _) <- ask
        i <- liftIO $ readIORef v
        liftIO $ modifyIORef' v (\j -> (j `mod` JE.maxSafeInteger) + 1)
        pure i
