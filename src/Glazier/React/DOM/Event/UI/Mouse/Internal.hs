{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.Event.UI.Mouse.Internal
( MouseEvent(..) -- ^ constructor is exported
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.DOM.Event
import Glazier.React.DOM.Event.Internal
import Glazier.React.DOM.Event.UI
import Glazier.React.DOM.Event.UI.Internal
import qualified JavaScript.Extras as JE

newtype MouseEvent =
    MouseEvent UIEvent
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS MouseEvent where
    validInstance = js_isMouseEvent
    fromJS a | js_isMouseEvent a = Just $ MouseEvent $ UIEvent $ Event a
    fromJS _ = Nothing

instance IEvent MouseEvent
instance IUIEvent MouseEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof MouseEvent"
    js_isMouseEvent :: J.JSVal -> Bool

#else

js_isMouseEvent :: J.JSVal -> Bool
js_isMouseEvent _ = False

#endif
