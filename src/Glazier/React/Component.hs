{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Component
    ( ReactComponent
    , mkReactComponent
    ) where

import Control.DeepSeq
import qualified Control.Disposable as CD
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import qualified GHCJS.Marshal.Pure as J
import qualified JavaScript.Extras as JE

-- | A newtype wrapper to give a noop dispose instance to React components
-- This allows generic deriving of Plan.
newtype ReactComponent = ReactComponent JE.JSVar
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance CD.Dispose ReactComponent where
    dispose _ = mempty

mkReactComponent :: IO ReactComponent
mkReactComponent = ReactComponent <$> js_mkReactComponent

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$component();"
  js_mkReactComponent
      :: IO JE.JSVar

#else

js_mkReactComponent :: IO JE.JSVar
js_mkReactComponent = pure $ JE.JSVar J.nullRef


#endif
