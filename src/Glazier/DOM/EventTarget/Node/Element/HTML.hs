{-# LANGUAGE CPP #-}

module Glazier.DOM.EventTarget.Node.Element.HTML
    ( HTMLElement -- ^ constructor not exported
    , IHTMLElement(..)
    ) where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget.Node.Element
import Glazier.DOM.EventTarget.Node.Element.HTML.Internal
import qualified JavaScript.Extras as JE
import Prelude hiding (id)

-- | https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
class IElement j => IHTMLElement j where
    focus :: MonadIO m => j -> m ()
    focus = liftIO . js_focus . JE.toJS

    blur :: MonadIO m => j -> m ()
    blur = liftIO . js_blur . JE.toJS

instance IHTMLElement HTMLElement

#ifdef __GHCJS__

foreign import javascript unsafe
    "if ($1 && $1['focus']) { $1['focus'](); }"
    js_focus :: J.JSVal -> IO ()

foreign import javascript unsafe
    "if ($1 && $1['blur']) { $1['blur'](); }"
    js_blur :: J.JSVal -> IO ()

#else

js_focus :: J.JSVal -> IO ()
js_focus _ = pure ()

js_blur :: J.JSVal -> IO ()
js_blur _ = pure ()

#endif
