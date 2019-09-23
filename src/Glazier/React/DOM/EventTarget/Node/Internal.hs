{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.EventTarget.Node.Internal
    ( Node(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.DOM.EventTarget
import qualified JavaScript.Extras as JE

newtype Node = Node J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS Node where
    fromJS a | js_isNode a = Just $ Node a
    fromJS _ = Nothing

instance IEventTarget Node

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof Node"
    js_isNode :: J.JSVal -> Bool

#else

js_isNode :: J.JSVal -> Bool
js_isNode _ = False

#endif
