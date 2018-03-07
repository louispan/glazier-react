{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glazier.React.Reactor.Run (
    IOReactor(..)
    ) where

import qualified Control.Disposable as CD
import Control.Monad.Reader
import Data.Function
import Data.IORef
import Data.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified Glazier.React.Component as Z
import qualified Glazier.React.Handle as Z
import qualified Glazier.React.Markup as Z
import Glazier.React.Reactor as Z
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

newtype IOReactor a = IOReactor
    { runIOReactor :: ReaderT (IORef Int) IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader (IORef Int)
               )


instance MonadReactor IOReactor where
    doNewIORef = liftIO . newIORef
    doReadIORef = liftIO . readIORef
    doWriteIORef v a = liftIO $ writeIORef v a
    doModifyIORef' v f = liftIO $ modifyIORef' v f
    -- doModifyIORefM v f = do
    --     env <- ask
    --     liftIO $ do
    --         x <- readIORef v
    --         x' <- (env &) . runReaderT . runIOReactor $ f x
    --         writeIORef v x'
    doMkCallback goStrict goLazy = do
        env <- ask
        let goLazy' = (env &) . runReaderT . runIOReactor . goLazy
        let f = Z.handleEventM goStrict goLazy'
        liftIO $ do
            cb <- J.syncCallback1 J.ContinueAsync (f . JE.JSRep)
            let d = CD.dispose cb in pure (d, cb)
    doMkRenderer rnd = do
        env <- ask
        liftIO $ do
            cb <- J.syncCallback' ((env &) . runReaderT . runIOReactor $ JE.toJS <$> Z.toElement rnd)
            let d = CD.dispose cb in pure (d, cb)
    doGetComponent = liftIO Z.mkReactComponent
    doMkSeq = do
        v <- ask
        i <- liftIO $ readIORef v
        liftIO $ modifyIORef' v (\j -> (j `mod` JE.maxSafeInteger) + 1)
        pure i
    doDispose d = liftIO $ fromMaybe (pure ()) (CD.runDisposable d)
    doSetComponentState p j = liftIO $ js_setComponentState p j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_setComponentState :: JO.Object -> Z.ReactComponent -> IO ()

#else

js_setComponentState :: JO.Object -> Z.ReactComponent -> IO ()
js_setComponentState _ _ = pure ()

#endif
