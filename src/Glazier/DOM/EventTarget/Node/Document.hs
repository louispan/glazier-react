{-# LANGUAGE CPP #-}

module Glazier.DOM.EventTarget.Node.Document
    ( -- | constructor not exported
      Document
    , IDocument(..)
    , globalDocument
    ) where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget.Node
import Glazier.DOM.EventTarget.Node.Document.Internal
import Glazier.DOM.EventTarget.Node.Element
import Glazier.DOM.EventTarget.Window.Internal
import qualified JavaScript.Extras as JE

-- | https://developer.mozilla.org/en-US/docs/Web/API/Document
class INode j => IDocument j where
    defaultView :: j -> IO (Maybe Window)
    defaultView = liftIO . fmap JE.fromJS . js_defaultView . JE.toJS

    getElementById :: MonadIO m => j -> J.JSString -> m (Maybe Element)
    getElementById j i = liftIO $ JE.fromJS <$> js_getElementById (JE.toJS j) i

instance IDocument Document

globalDocument :: Maybe Document
globalDocument = JE.fromJS js_document

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = globalThis.document"
    js_document :: J.JSVal

foreign import javascript unsafe
    "$r = $1.defaultView"
    js_defaultView :: J.JSVal -> IO J.JSVal

foreign import javascript unsafe
    "$r = $1.getElementById($2)"
    js_getElementById :: J.JSVal -> J.JSString -> IO J.JSVal

#else

js_document :: J.JSVal
js_document = J.nullRef

js_defaultView :: J.JSVal -> IO J.JSVal
js_defaultView _ = pure J.nullRef

js_getElementById :: J.JSVal -> J.JSString -> IO J.JSVal
js_getElementById _ _ = pure J.nullRef

#endif
