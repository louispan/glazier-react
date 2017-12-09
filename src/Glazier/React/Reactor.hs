{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Reactor where

import Control.DeepSeq
import Data.IORef
import qualified Data.JSString as J
import Data.String
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Dispose as R
import qualified Glazier.React.Markup as R
import JavaScript.Extras.Cast as JE

newtype Renderer = Renderer { runRenderer :: J.Callback (IO J.JSVal) }
    deriving (R.Dispose, JE.ToJS, J.IsJSVal)
newtype ReactKey = ReactKey { runReactKey :: J.JSString }
    deriving (Read, Show, Eq, Ord, R.Dispose, JE.ToJS, JE.FromJS, IsString, J.IsJSVal, J.PToJSVal)

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

mkReactKey :: MonadReactor m => m ReactKey
mkReactKey = (ReactKey . J.pack . show) <$> mkReactKey
