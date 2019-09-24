{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.DOM.EventTarget.Window.Internal
    ( Window(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.DOM.EventTarget
import qualified JavaScript.Extras as JE

newtype Window = Window J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS Window where
    fromJS a | js_isWindow a = Just $ Window a
    fromJS _ = Nothing

instance IEventTarget Window

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof Window"
    js_isWindow :: J.JSVal -> Bool

#else

js_isWindow :: J.JSVal -> Bool
js_isWindow _ = False

#endif
