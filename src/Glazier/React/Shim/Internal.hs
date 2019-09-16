{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Shim.Internal
    ( ShimComponent(..) -- constructor exported
    , shimComponent
    , rerenderShim
    , batchShimRerender
    , ShimRef(..) -- constructor exported
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React.ReactBatch
import qualified JavaScript.Extras as JE

-- | Returns a reference to the javascript *class* definition
-- of the shim wrapper around ReactPureComponent
newtype ShimComponent = ShimComponent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

-- | This returns the javascript class definition of ShimComponent.
-- There is ever only one shim class, so it is purely available
shimComponent :: ShimComponent
shimComponent = ShimComponent js_shimComponent

-- | Rerenders an instance of a component created using ShimComponent.
rerenderShim :: ShimRef -> IO ()
rerenderShim = js_rerenderShim

-- | batch rerendering of a shim
batchShimRerender :: ShimRef -> ReactBatch -> IO ()
batchShimRerender b s = js_batchShimRerender (JE.toJS s) (JE.toJS b)

-- | This is used store the react "ref" to a javascript instance of a react Component.
newtype ShimRef = ShimRef J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS ShimRef where
    fromJS a | js_isReactComponent a = Just $ ShimRef a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$ShimComponent();"
  js_shimComponent :: J.JSVal

-- !!blah is javascript way of converting to bool
-- using undocumented api to check if something is react component
-- https://stackoverflow.com/questions/33199959/how-to-detect-a-react-component-vs-a-react-element
foreign import javascript unsafe
  "!(!($1 && !(!($1['isReactComponent']))))"
  js_isReactComponent :: J.JSVal -> Bool

foreign import javascript unsafe
  "if ($1 && $1['rerender']){$1['rerender']()};"
  js_rerenderShim :: ShimRef -> IO ()

foreign import javascript unsafe
  "if ($1 && $1['rerender'] && $2 && $2['batch']){$2['batch'](function(){$1['rerender']()})};"
  js_batchShimRerender :: J.JSVal -> J.JSVal -> IO ()

#else

js_shimComponent :: J.JSVal
js_shimComponent = J.nullRef

js_isReactComponent :: J.JSVal -> Bool
js_isReactComponent _ = False

js_rerenderShim :: ShimRef -> IO ()
js_rerenderShim _ = pure ()

js_batchShimRerender :: J.JSVal -> J.JSVal -> IO ()
js_batchShimRerender _ _ = pure ()

#endif
