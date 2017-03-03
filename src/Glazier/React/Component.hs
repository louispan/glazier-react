{-# LANGUAGE OverloadedStrings #-}

-- | Contains commons utilities when defining your own widget
module Glazier.React.Component
  ( onRender
  , onRef
  , onUpdated
  , shimComponent
  ) where

import Control.Concurrent.MVar
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified GHCJS.Extras as E
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R

-- | This is called synchronously by React to render the DOM.
-- This must not block!
onRender :: MVar s -> G.WindowT s (R.ReactMlT IO) () -> IO J.JSVal
onRender ms wdw = do
    s <- readMVar ms
    J.pToJSVal <$> R.markedElement wdw s

onRef :: (J.JSVal -> act) -> J.JSVal -> MaybeT IO act
onRef f = pure . f

onUpdated :: (Int -> act) -> J.JSVal -> MaybeT IO act
onUpdated f =  R.eventHandlerM goStrict goLazy
  where
    -- goStrict :: J.JSVal -> MaybeT IO Int
    goStrict v = do
        i <- lift $ E.getProperty "frameNum" v
        MaybeT $ J.fromJSVal i

    -- goLazy :: Int -> MaybeT IO act
    goLazy i = pure $ f i

-- | Get a reference to the Shim react component
shimComponent :: J.JSVal
shimComponent = js_shimComponent

-- | This expect Shim to be in global scope.
-- See examples/todo/haskell/exports.header
foreign import javascript unsafe
  "$r = Shim;"
  js_shimComponent
      :: J.JSVal
