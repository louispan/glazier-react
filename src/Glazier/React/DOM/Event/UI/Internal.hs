{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.Event.UI.Internal
( UIEvent(..) -- ^ constructor is exported
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.DOM.Event
import qualified JavaScript.Extras as JE

-- | https://developer.mozilla.org/en-US/docs/Web/API/UIEvent
newtype UIEvent =
    UIEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS UIEvent where
    validInstance = js_isUIEvent
    fromJS a | js_isUIEvent a = Just $ UIEvent a
    fromJS _ = Nothing

instance IEvent UIEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 instanceof UIEvent"
    js_isUIEvent :: J.JSVal -> Bool

#else

js_isUIEvent :: J.JSVal -> Bool
js_isUIEvent _ = False

#endif
