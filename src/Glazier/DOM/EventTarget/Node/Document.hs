{-# LANGUAGE CPP #-}

module Glazier.DOM.EventTarget.Node.Document
    ( Document -- ^ constructor not exported
    , IDocument(..)
    , globalDocument
    ) where

import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget.Node
import Glazier.DOM.EventTarget.Node.Document.Internal
import Glazier.DOM.EventTarget.Window.Internal
import qualified JavaScript.Extras as JE

-- | https://developer.mozilla.org/en-US/docs/Web/API/Document
class INode j => IDocument j where
    defaultView :: j -> Maybe Window
    defaultView = JE.fromJS . js_defaultView . JE.toJS

instance IDocument Document

globalDocument :: Maybe Document
globalDocument = JE.fromJS js_document

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = document"
    js_document :: J.JSVal

foreign import javascript unsafe
    "$r = $1['defaultView']"
    js_defaultView :: J.JSVal -> J.JSVal

#else

js_document :: J.JSVal
js_document = J.nullRef

js_defaultView :: J.JSVal -> J.JSVal
js_defaultView _ = J.nullRef

#endif
