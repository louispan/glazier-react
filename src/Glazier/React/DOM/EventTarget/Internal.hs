{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.EventTarget.Internal
    ( EventTarget(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

newtype EventTarget = EventTarget J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS EventTarget where
    validInstance = js_isEventTarget
    fromJS a | js_isEventTarget a = Just $ EventTarget a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof EventTarget"
    js_isEventTarget :: J.JSVal -> Bool

#else

js_isEventTarget :: J.JSVal -> Bool
js_isEventTarget _ = False

#endif
