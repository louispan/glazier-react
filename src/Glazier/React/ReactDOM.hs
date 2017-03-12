{-# LANGUAGE CPP #-}

-- | Contains commons utilities when defining your own widget
module Glazier.React.ReactDOM
  ( render
  ) where

import qualified GHCJS.Types as J

-- | Using a React Element (first arg) give React rendering control over a DOM element (second arg).
-- This should only be called for the topmost component.
render :: J.JSVal -> J.JSVal -> IO ()
render = js_render

#ifdef __GHCJS__

foreign import javascript unsafe
  "ReactDOM.render($1, $2);"
  js_render :: J.JSVal -> J.JSVal -> IO ()

#else

js_render :: J.JSVal -> J.JSVal -> IO ()
js_render _ _ = pure ()

#endif
