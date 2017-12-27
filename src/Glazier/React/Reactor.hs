{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.React.Reactor where

import Control.DeepSeq
import Control.Disposable as CD
import qualified Data.DList as DL
import Data.IORef
import qualified Data.JSString as JS
import Data.String
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R
import JavaScript.Extras.Cast as JE

-- | x is the type of execution commands
class Monad m =>
      MonadReactor x m | m -> x where
    doNewIORef :: a -> m (IORef a)
    doReadIORef :: IORef a -> m a
    doWriteIORef :: IORef a -> a -> m ()
    doModifyIORef' :: IORef a -> (a -> a) -> m ()
    doModifyIORefM :: IORef a -> (a -> m a) -> m ()
    mkCallback
        :: (NFData a)
        => (J.JSVal -> IO a) -- generate event strictly
        -> (a -> m (DL.DList x)) -- produce final execution lazily
        -> m (J.Callback (J.JSVal -> IO ()))
    mkRenderer :: R.ReactMlT m () -> m (J.Callback (IO J.JSVal))
    getComponent :: m R.ReactComponent
    mkSeq :: m Int

newtype ReactKey = ReactKey { runReactKey :: J.JSString }
    deriving (Read, Show, Eq, Ord, CD.Dispose, JE.ToJS, JE.FromJS, IsString, J.IsJSVal, J.PToJSVal)

mkReactKey :: MonadReactor x m => JS.JSString -> m ReactKey
mkReactKey n = (ReactKey . JS.append n . JS.cons ':' . JS.pack . show) <$> mkSeq
