{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.DOM.EventTarget.Node.Element.Internal
    ( Element(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget
import Glazier.DOM.EventTarget.Node
import qualified JavaScript.Extras as JE

newtype Element = Element J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS Element where
    fromJS a | js_isElement a = Just $ Element a
    fromJS _ = Nothing

instance IEventTarget Element

instance INode Element

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof Element"
    js_isElement :: J.JSVal -> Bool

#else

js_isElement :: J.JSVal -> Bool
js_isElement _ = False

#endif
