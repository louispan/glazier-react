{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.EventTarget.Node.Element.HTMLElement
    ( HTMLElement(..)
    ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.DOM.EventTarget.Internal
import Glazier.React.DOM.EventTarget.Node.Element
import Glazier.React.DOM.EventTarget.Node.Internal
import qualified JavaScript.Extras as JE

-- | https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
-- Not hiding constructor, so you can coerce to 'Element'
-- It is not safe to coerce from 'Element'
newtype HTMLElement = HTMLElement Element
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS HTMLElement where
    fromJS a | js_isHTMLElement a = Just $ HTMLElement $ Element $ Node $ EventTarget a
    fromJS _ = Nothing

focus :: MonadIO m => HTMLElement -> m ()
focus j = liftIO $ js_blur j

blur :: MonadIO m => HTMLElement -> m ()
blur j = liftIO $ js_focus j

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 instanceof HTMLElement"
    js_isHTMLElement :: J.JSVal -> Bool

foreign import javascript unsafe
    "if ($1 && $1['focus']) { $1['focus'](); }"
    js_focus :: HTMLElement -> IO ()

foreign import javascript unsafe
    "if ($1 && $1['blur']) { $1['blur'](); }"
    js_blur :: HTMLElement -> IO ()

#else

js_isHTMLElement :: J.JSVal -> Bool
js_isHTMLElement _ = False

js_focus :: HTMLElement -> IO ()
js_focus _ = pure ()

js_blur :: HTMLElement -> IO ()
js_blur _ = pure ()

#endif
