module Glazier.React.Reactor where

import Data.IORef
import qualified Data.JSString as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Dispose as R

class Monad m => MonadReactor m where
    doNewIORef :: a -> m (IORef a)
    doReadIORef :: IORef a -> m a
    doWriteIORef :: IORef a -> a -> m ()
    mkCallback :: (J.JSVal -> m ()) -> m (J.Callback (J.JSVal -> IO ()))
    mkRenderer :: R.ReactMlT m () -> m (J.Callback (IO J.JSVal))
    getComponent :: m R.ReactComponent
    mkKey :: m Int
    doDispose :: R.Disposable () -> m ()

mkKey' :: MonadReactor m => m J.JSString
mkKey' = (J.pack . show) <$> mkKey
