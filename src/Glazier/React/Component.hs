{-# LANGUAGE OverloadedStrings #-}

-- | Contains commons utilities when defining your own widget
module Glazier.React.Component
    ( onRender
    , shimComponent
    ) where

import Control.Concurrent.MVar
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R

-- | This is called synchronously by React to render the DOM.
-- This must not block!
onRender :: MVar s -> (J.JSVal -> G.WindowT s (R.ReactMlT IO) ()) -> J.JSVal -> IO J.JSVal
onRender ms wdw v = do
    s <- readMVar ms
    J.pToJSVal <$> R.markedElement (wdw v) s

-- | Get a reference to the Shim react component
shimComponent :: J.JSVal
shimComponent = js_shimComponent

-- | This expect Shim to be in global scope.
-- See examples/todo/haskell/exports.header
foreign import javascript unsafe
  "$r = Shim;"
  js_shimComponent
      :: J.JSVal
