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
import JS.Data

-- | Contains an instance of something that can batch react state updates
newtype ReactBatch = ReactBatch JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

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
  js_mkReactBatch :: IO JSVal

foreign import javascript unsafe
  "$1.runBatch();"
  js_runReactBatch :: JSVal -> IO ()

#else

js_mkReactBatch :: IO JSVal
js_mkReactBatch = pure nullRef

js_runReactBatch :: JSVal -> IO ()
js_runReactBatch _ = pure ()

#endif
