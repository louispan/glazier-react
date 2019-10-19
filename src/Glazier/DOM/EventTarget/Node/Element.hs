{-# LANGUAGE CPP #-}

module Glazier.DOM.EventTarget.Node.Element
    ( -- | constructor not exported
      Element
    , IElement(..)
    ) where

import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget.Node
import Glazier.DOM.EventTarget.Node.Element.Internal
import qualified JavaScript.Extras as JE
import Prelude hiding (id)

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element
class INode j => IElement j where
    className :: j -> J.JSString
    className = js_className . JE.toJS

    id :: j -> J.JSString
    id = js_id . JE.toJS

instance IElement Element

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['className']"
    js_className :: J.JSVal -> J.JSString

foreign import javascript unsafe
    "$r = $1['id']"
    js_id :: J.JSVal -> J.JSString

#else

js_className :: J.JSVal -> J.JSString
js_className _ = mempty

js_id :: J.JSVal -> J.JSString
js_id _ = mempty

#endif
