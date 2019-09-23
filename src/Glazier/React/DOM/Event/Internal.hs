{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.DOM.Event.Internal
( Event(..) -- ^ constructor is exported
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | The native event
newtype Event =
    Event J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS Event where
    validInstance = js_isEvent
    fromJS a | js_isEvent a = Just $ Event a
    fromJS _ = Nothing


#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 != undefined && $1 instanceof Event"
    js_isEvent :: J.JSVal -> Bool

#else

js_isEvent :: J.JSVal -> Bool
js_isEvent _ = False

#endif
