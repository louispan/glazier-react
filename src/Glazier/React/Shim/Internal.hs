{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Shim.Internal
    ( ShimComponent(..) -- constructor exported
    , shimComponent
    , rerenderShim
    , ShimRef(..) -- constructor exported
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

-- | This returns the javascript class definition of ShimComponent.
-- There is ever only one shim class, so it is purely available
shimComponent :: ShimComponent
shimComponent = ShimComponent js_shimComponent

-- | Rerenders an instance of a component created using ShimComponent.
rerenderShim :: ShimRef -> IO ()
rerenderShim = js_rerenderShim

-- | This is used store the react "ref" to a javascript instance of a react Component.
newtype ShimRef = ShimRef JE.JSRep
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS ShimRef where
    fromJS a | js_isReactComponent a = Just $ ShimRef $ JE.JSRep a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$shimComponent();"
  js_shimComponent :: JE.JSRep

-- !!blah is javascript way of converting to bool
-- using undocumented api to check if something is react component
-- https://stackoverflow.com/questions/33199959/how-to-detect-a-react-component-vs-a-react-element
foreign import javascript unsafe
    "!(!($1 && !(!($1['isReactComponent']))))"
    js_isReactComponent :: J.JSVal -> Bool

foreign import javascript unsafe
  "if ($1 && $1['rerender']) { $1['rerender']() };"
  js_rerenderShim :: ShimRef -> IO ()

#else

js_shimComponent :: JE.JSRep
js_shimComponent = JE.JSRep J.nullRef

js_isReactComponent :: J.JSVal -> Bool
js_isReactComponent _ = False

js_rerenderShim :: ShimRef -> IO ()
js_rerenderShim _ = pure ()

#endif
