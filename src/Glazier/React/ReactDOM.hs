{-# LANGUAGE CPP #-}

-- | Contains commons utilities when defining your own widget
module Glazier.React.ReactDOM
  ( renderDOM
  ) where

import qualified Glazier.DOM.EventTarget.Node.Element as DOM
import Glazier.React.ReactElement

-- | Using a React Element (first arg) give React rendering control over a DOM element (second arg).
-- This should only be called for the topmost component.
renderDOM :: ReactElement -> DOM.Element -> IO ()
renderDOM = js_render

#ifdef __GHCJS__

foreign import javascript unsafe
  "hgr$ReactDOM().render($1, $2);"
  js_render :: ReactElement -> DOM.Element -> IO ()

#else

js_render :: ReactElement -> DOM.Element -> IO ()
js_render _ _ = pure ()

#endif
