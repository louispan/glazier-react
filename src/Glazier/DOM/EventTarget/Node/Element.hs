{-# LANGUAGE CPP #-}

module Glazier.DOM.EventTarget.Node.Element
    ( -- | constructor not exported
      Element
    , IElement(..)
    ) where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget.Node
import Glazier.DOM.EventTarget.Node.Element.Internal
import qualified JavaScript.Extras as JE
import Prelude hiding (id)

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element
class INode j => IElement j where
    className :: MonadIO m => j -> m J.JSString
    className = liftIO . js_className . JE.toJS

    id :: MonadIO m => j -> m J.JSString
    id = liftIO . js_id . JE.toJS

instance IElement Element

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['className']"
    js_className :: J.JSVal -> IO J.JSString

foreign import javascript unsafe
    "$r = $1['id']"
    js_id :: J.JSVal -> IO J.JSString

#else

js_className :: J.JSVal -> IO J.JSString
js_className _ = pure mempty

js_id :: J.JSVal -> IO J.JSString
js_id _ = pure mempty

#endif
