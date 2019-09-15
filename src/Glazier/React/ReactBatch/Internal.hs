{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactBatch.Internal
    ( ReactBatch(..) -- constructor exported
    , mkReactBatch
    , runReactBatch
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | Contains an instance of something that can batch react state updates
newtype ReactBatch = ReactBatch J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

-- | This an instance of  ShimComponent.
-- There is ever only one shim class, so it is purely available
mkReactBatch :: IO ReactBatch
mkReactBatch = ReactBatch <$> js_mkReactBatch

-- | run all the currently batched rerenders
runReactBatch :: ReactBatch -> IO ()
runReactBatch (ReactBatch b) = js_runReactBatch b

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$mkReactBatcher();"
  js_mkReactBatch :: IO J.JSVal

foreign import javascript unsafe
  "if ($1 && $1['runBatch']){$1['runBatch']()};"
  js_runReactBatch :: J.JSVal -> IO ()

#else

js_mkReactBatch :: IO J.JSVal
js_mkReactBatch = pure J.nullRef

js_runReactBatch :: J.JSVal -> IO ()
js_runReactBatch _ = pure ()

#endif
