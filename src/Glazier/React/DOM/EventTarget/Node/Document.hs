{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.EventTarget.Node.Document
    ( Document(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.DOM.EventTarget.Internal
import Glazier.React.DOM.EventTarget.Node.Internal
import qualified JavaScript.Extras as JE

-- | https://developer.mozilla.org/en-US/docs/Web/API/Document
-- Not hiding constructor, so you can coerce to 'Node'
-- It is not safe to coerce from 'Node'
newtype Document = Document Node
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS Document where
    fromJS a | js_isDocument a = Just $ Document $ Node $ EventTarget a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 instanceof Document"
    js_isDocument :: J.JSVal -> Bool

#else

js_isDocument :: J.JSVal -> Bool
js_isDocument _ = False

#endif
