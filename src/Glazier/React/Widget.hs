{-# LANGUAGE OverloadedStrings #-}

-- | Contains commons utilities when defining your own widget
module Glazier.React.Widget
  ( onRender
  , onRef
  , onUpdated
  , shimComponent
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified GHCJS.Marshal as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Element as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E

-- | This is called synchronously by React to render the DOM.
-- This must not block!
onRender :: G.WindowT s (R.ReactMlT IO) () -> MVar s -> IO J.JSVal
onRender w s = do
    s' <- readMVar s
    xs <- view G._WindowT' (R.renderedWindow w) s'
    J.jsval <$> R.mkCombinedElements xs

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
