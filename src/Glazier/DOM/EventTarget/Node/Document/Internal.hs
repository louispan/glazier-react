{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.DOM.EventTarget.Node.Document.Internal
    ( Document(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget
import qualified JavaScript.Extras as JE

newtype Document = Document J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS Document where
    fromJS a | js_isDocument a = Just $ Document a
    fromJS _ = Nothing

instance IEventTarget Document

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Document"
    js_isDocument :: J.JSVal -> Bool

#else

js_isDocument :: J.JSVal -> Bool
js_isDocument _ = False

#endif
