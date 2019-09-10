{-# LANGUAGE CPP #-}

-- | Contains commons utilities when defining your own widget
module Glazier.React.ReactDOM
  ( renderDOM
  ) where

import qualified GHCJS.Types as J
import qualified Glazier.React.Element as Z

-- | Using a React Element (first arg) give React rendering control over a DOM element (second arg).
-- This should only be called for the topmost component.
renderDOM :: Z.ReactElement -> J.JSVal -> IO ()
renderDOM = js_render

#ifdef __GHCJS__

foreign import javascript unsafe
  "hgr$ReactDOM().render($1, $2);"
  js_render :: Z.ReactElement-> J.JSVal -> IO ()

#else

js_render :: Z.ReactElement -> J.JSVal -> IO ()
js_render _ _ = pure ()

#endif
