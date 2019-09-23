{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.Event.HashChange.Internal
( HashChangeEvent(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.DOM.Event
import qualified JavaScript.Extras as JE

newtype HashChangeEvent = HashChangeEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS HashChangeEvent where
    validInstance = js_isHashChangeEvent
    fromJS a | js_isHashChangeEvent a = Just $ HashChangeEvent a
    fromJS _ = Nothing

instance IEvent HashChangeEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof HashChangeEvent"
    js_isHashChangeEvent :: J.JSVal -> Bool

#else

js_isHashChangeEvent :: J.JSVal -> Bool
js_isHashChangeEvent _ = False

#endif
