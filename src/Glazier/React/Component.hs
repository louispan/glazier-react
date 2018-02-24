{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Component
    ( ReactComponent
    , mkReactComponent
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | A newtype wrapper to give a noop dispose instance to React components
-- This allows generic deriving of Plan.
newtype ReactComponent = ReactComponent JE.JSRep
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

mkReactComponent :: IO ReactComponent
mkReactComponent = ReactComponent <$> js_mkReactComponent

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$component();"
  js_mkReactComponent
      :: IO JE.JSRep

#else

js_mkReactComponent :: IO JE.JSRep
js_mkReactComponent = pure $ JE.JSRep J.nullRef


#endif
