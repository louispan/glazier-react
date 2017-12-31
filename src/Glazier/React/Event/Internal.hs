{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Event.Internal where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | The object that dispatched the event.
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
newtype EventTarget =
    EventTarget JE.JSVar
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS EventTarget where
    fromJS a | js_isEventTarget a = Just $ EventTarget $ JE.JSVar a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 instanceof EventTarget"
    js_isEventTarget :: J.JSVal -> Bool

#else

js_isEventTarget :: J.JSVal -> Bool
js_isEventTarget _ = False

#endif
