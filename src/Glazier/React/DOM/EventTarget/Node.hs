{-# LANGUAGE CPP #-}

module Glazier.React.DOM.EventTarget.Node
    ( Node
    , INode(..)
    ) where

import qualified GHCJS.Types as J
import Glazier.React.DOM.EventTarget
import Glazier.React.DOM.EventTarget.Node.Internal
import qualified JavaScript.Extras as JE

class IEventTarget j => INode j where
    nodeName :: j -> J.JSString
    nodeName = js_nodeName . JE.toJS

    nodeType :: j -> Int
    nodeType = js_nodeType . JE.toJS

instance INode Node

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['nodeName']"
    js_nodeName :: J.JSVal -> J.JSString

foreign import javascript unsafe
    "$r = $1['nodeType']"
    js_nodeType :: J.JSVal -> Int

#else

js_nodeName :: J.JSVal -> J.JSString
js_nodeName _ = mempty

js_nodeType :: J.JSVal -> Int
js_nodeType _ = 0

#endif
