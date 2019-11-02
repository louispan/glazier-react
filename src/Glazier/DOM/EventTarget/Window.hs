{-# LANGUAGE CPP #-}

module Glazier.DOM.EventTarget.Window
    ( -- | constructor not exported
      Window
    , IWindow(..)
    , globalWindow
    ) where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget
import Glazier.DOM.EventTarget.Node.Document.Internal
import Glazier.DOM.EventTarget.Window.Internal
import qualified JavaScript.Extras as JE

-- https://developer.mozilla.org/en-US/docs/Web/API/Window
class IEventTarget j => IWindow j where
    document :: MonadIO m => j -> m (Maybe Document)
    document = liftIO . fmap JE.fromJS . js_document . JE.toJS

instance IWindow Window

globalWindow :: Maybe Window
globalWindow = JE.fromJS js_window

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = globalThis.window"
    js_window :: J.JSVal

foreign import javascript unsafe
    "$r = $1['document']"
    js_document :: J.JSVal -> IO J.JSVal

#else

js_window :: J.JSVal
js_window = J.nullRef

js_document :: J.JSVal -> IO J.JSVal
js_document _ = pure J.nullRef

#endif
