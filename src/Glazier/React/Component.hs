{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Component
    ( ReactComponent
    , mkComponent
    ) where

import Control.DeepSeq
import qualified Control.Disposable as CD
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import qualified GHCJS.Marshal.Pure as J
import qualified JavaScript.Extras as JE

-- | A newtype wrapper to give a noop disposable instance to React components
-- This allows generic deriving of Plan.
newtype ReactComponent = ReactComponent JE.JSVar
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, JE.FromJS, IsString, NFData)

instance CD.Disposing ReactComponent where
    disposing _ = CD.DisposeNone

mkComponent :: IO ReactComponent
mkComponent = ReactComponent <$> js_mkComponent

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$mkComponent();"
  js_mkComponent
      :: IO JE.JSVar

#else

js_mkComponent :: IO JE.JSVar
js_mkComponent = pure $ JE.JSVar J.nullRef


#endif
