{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.EventTarget.Node.Internal
    ( Node(..)
    , fromNode
    ) where

import Control.DeepSeq
import Data.Coerce
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.DOM.EventTarget.Internal
import qualified JavaScript.Extras as JE

-- | https://developer.mozilla.org/en-US/docs/Web/API/Node
-- Not hiding constructor, so you can coerce to 'EventTarget'
-- It is not safe to coerce from 'EventTarget'
newtype Node = Node EventTarget
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS Node where
    fromJS a | js_isNode a = Just $ Node $ EventTarget a
    fromJS _ = Nothing

fromNode :: Coercible Node b => Node -> b
fromNode = coerce

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 instanceof Node"
    js_isNode :: J.JSVal -> Bool

#else

js_isNode :: J.JSVal -> Bool
js_isNode _ = False

#endif
