module Glazier.React.Reactor where

import Control.DeepSeq
import Data.IORef
import qualified Data.JSString as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R

class Monad m =>
      MonadReactor m where
    doNewIORef :: a -> m (IORef a)
    doReadIORef :: IORef a -> m a
    doWriteIORef :: IORef a -> a -> m ()
    mkCallback
        :: (NFData a)
        => (J.JSVal -> IO a) -- generate event
        -> (a -> m b) -- transform with limited effects
        -> (b -> IO ()) -- final execution in IO
        -> m (J.Callback (J.JSVal -> IO ()))
    mkRenderer :: R.ReactMlT m () -> m (J.Callback (IO J.JSVal))
    getComponent :: m R.ReactComponent
    mkKey :: m Int

mkKey' :: MonadReactor m => m J.JSString
mkKey' = (J.pack . show) <$> mkKey
