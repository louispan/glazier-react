{-# LANGUAGE CPP #-}

-- | Contains commons utilities when defining your own widget
module Glazier.React.ReactDOM
  ( renderDOM
  ) where

import qualified Glazier.React.Element as R
import qualified JavaScript.Extras as JE

-- | Using a React Element (first arg) give React rendering control over a DOM element (second arg).
-- This should only be called for the topmost component.
renderDOM :: R.ReactElement -> JE.JSRep -> IO ()
renderDOM = js_render

#ifdef __GHCJS__

foreign import javascript unsafe
  "hgr$ReactDOM().render($1, $2);"
  js_render :: R.ReactElement-> JE.JSRep -> IO ()

#else

js_render :: R.ReactElement -> JE.JSRep -> IO ()
js_render _ _ = pure ()

#endif
