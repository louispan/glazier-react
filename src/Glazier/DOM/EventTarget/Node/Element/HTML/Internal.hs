{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.DOM.EventTarget.Node.Element.HTML.Internal
    ( HTMLElement(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget
import Glazier.DOM.EventTarget.Node
import Glazier.DOM.EventTarget.Node.Element
import qualified JavaScript.Extras as JE

newtype HTMLElement = HTMLElement J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS HTMLElement where
    fromJS a | js_isHTMLElement a = Just $ HTMLElement a
    fromJS _ = Nothing

instance IEventTarget HTMLElement

instance INode HTMLElement

instance IElement HTMLElement

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof HTMLElement"
    js_isHTMLElement :: J.JSVal -> Bool

#else

js_isHTMLElement :: J.JSVal -> Bool
js_isHTMLElement _ = False

#endif
