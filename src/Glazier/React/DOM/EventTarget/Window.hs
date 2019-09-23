{-# LANGUAGE CPP #-}

module Glazier.React.DOM.EventTarget.Window
    ( Window -- ^ constructor not exported
    , IWindow(..)
    , window
    ) where

import qualified GHCJS.Types as J
import Glazier.React.DOM.EventTarget
import Glazier.React.DOM.EventTarget.Node.Document.Internal
import Glazier.React.DOM.EventTarget.Window.Internal
import qualified JavaScript.Extras as JE

-- https://developer.mozilla.org/en-US/docs/Web/API/Window
class IEventTarget j => IWindow j where
    document :: j -> Maybe Document
    document = JE.fromJS . js_document . JE.toJS

instance IWindow Window

window :: Maybe Window
window = JE.fromJS js_window

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = window"
    js_window :: J.JSVal

foreign import javascript unsafe
    "$r = $1['document']"
    js_document :: J.JSVal -> J.JSVal

#else

js_window :: J.JSVal
js_window = J.nullRef

js_document :: J.JSVal -> J.JSVal
js_document _ = J.nullRef

#endif
