{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Reactor where

import Control.DeepSeq
import Data.IORef
import qualified Data.JSString as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Dispose as R
import qualified Glazier.React.Markup as R

newtype Renderer = Renderer { runRenderer :: J.Callback (IO J.JSVal) } deriving R.Dispose
newtype Key = Key { runKey :: J.JSString } deriving (Read, Show, Eq, Ord, R.Dispose)

class Monad m =>
      MonadReactor m where
    doNewIORef :: a -> m (IORef a)
    doReadIORef :: IORef a -> m a
    doWriteIORef :: IORef a -> a -> m ()
    doModifyIORef' :: IORef a -> (a -> a) -> m ()
    mkIO :: (a -> m b) -> m (a -> IO b)
    mkCallback
        :: (NFData a)
        => (J.JSVal -> IO a) -- generate event
        -> (a -> IO ()) -- final execution in IO
        -> m (J.Callback (J.JSVal -> IO ()))
    mkRenderer :: R.ReactMlT m () -> m Renderer
    getComponent :: m R.ReactComponent
    mkSeq :: m Int

mkKey :: MonadReactor m => m Key
mkKey = (Key . J.pack . show) <$> mkKey
