{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Reactor where

import Control.DeepSeq
import qualified Control.Disposable as CD
import Data.IORef
import qualified Data.JSString as J
import Data.String
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

-- | x is the type of execution commands
-- This contains the base allowed effects within MonadReactor.
-- All other extra effects must be the type of @x@.
class Monad m => MonadReactor m where
    doNewIORef :: a -> m (IORef a)
    doReadIORef :: IORef a -> m a
    doWriteIORef :: IORef a -> a -> m ()
    doModifyIORef' :: IORef a -> (a -> a) -> m ()
    doModifyIORefM :: IORef a -> (a -> m a) -> m ()
    doMkCallback :: (NFData a)
        => (JE.JSRep -> IO a) -- generate event strictly
        -> (a -> m ()) -- produce final execution lazily
        -> m (CD.Disposable, J.Callback (J.JSVal -> IO ()))
    doMkRenderer :: R.ReactMlT m () -> m (CD.Disposable, J.Callback (IO J.JSVal))
    doGetComponent :: m R.ReactComponent
    doMkSeq :: m Int
    doDispose :: CD.Disposable -> m ()
    doSetComponentState :: JO.Object -> R.ReactComponent -> m ()

newtype ReactKey = ReactKey { runReactKey :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, JE.FromJS, IsString, J.IsJSVal, J.PToJSVal)

doMkReactKey :: MonadReactor m => J.JSString -> m ReactKey
doMkReactKey n = (ReactKey . J.append n . J.cons ':' . J.pack . show) <$> doMkSeq
