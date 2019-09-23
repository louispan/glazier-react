{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.EventTarget.Node.Element.Internal
  ( Element(..)
  , ToElement(..)
  )
where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.DOM.EventTarget.Internal
import Glazier.React.DOM.EventTarget.Node.Internal
import qualified JavaScript.Extras as JE

-- https://developer.mozilla.org/en-US/docs/Web/API/Element
newtype Element = Element Node
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS Element where
    fromJS a | js_isElement a = Just $ Element $ Node $ EventTarget a
    fromJS _ = Nothing

class ToElement a where
    toElement :: a -> Element

instance ToElement Element where
    toElement = id

instance ToNode Node where
    toNode (Element a) = a

instance ToEventTarget Element where
    toEventTarget (Element a) = toEventTarget a

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof Element"
    js_isElement :: J.JSVal -> Bool

#else

js_isElement :: J.JSVal -> Bool
js_isElement _ = False

#endif
