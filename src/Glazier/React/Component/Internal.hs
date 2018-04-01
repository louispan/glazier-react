{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Component.Internal
    ( ShimComponent(..) -- constructor exported
    , shimComponent
    , ComponentRef(..) -- constructor exported
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | Returns a reference to the javascript *class* definition
-- of the shim wrapper around ReactPureComponent
newtype ShimComponent = ShimComponent JE.JSRep
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

-- | There is ever only one shim class, so it is purely available
shimComponent :: ShimComponent
shimComponent = ShimComponent js_shimComponent

-- | This is used store the react "ref" to a javascript instance
-- of a react Component, so that react "this.setState" can be called.
newtype ComponentRef = ComponentRef JE.JSRep
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS ComponentRef where
    fromJS a | js_isComponent a = Just $ ComponentRef $ JE.JSRep a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$shimComponent();"
  js_shimComponent
      :: JE.JSRep

foreign import javascript unsafe
    "$1 && $1['prototype'] && !(!($1['prototype']['isReactComponent']))"
    js_isComponent :: J.JSVal -> Bool

#else

js_shimComponent :: JE.JSRep
js_shimComponent = JE.JSRep J.nullRef

js_isComponent :: J.JSVal -> Bool
js_isComponent _ = False

#endif
