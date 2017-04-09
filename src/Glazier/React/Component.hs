{-# LANGUAGE CPP #-}

module Glazier.React.Component
    ( ReactComponent
    , mkComponent
    ) where

import qualified Control.Disposable as CD
import qualified GHCJS.Types as J
import qualified GHCJS.Marshal.Pure as J
import qualified JavaScript.Extras.Cast as JE

-- | A newtype wrapper to give a noop disposable instance to React components
-- This allows generic deriving of model Adaptors.
newtype ReactComponent = ReactComponent J.JSVal

instance CD.Disposing ReactComponent where
    disposing _ = CD.DisposeNone

instance J.IsJSVal ReactComponent
instance J.PToJSVal ReactComponent where
    pToJSVal = J.jsval
instance JE.ToJS ReactComponent

mkComponent :: IO ReactComponent
mkComponent = ReactComponent <$> js_mkComponent

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$shimComponent();"
  js_mkComponent
      :: IO J.JSVal

#else

js_mkComponent :: IO J.JSVal
js_mkComponent = pure J.nullRef


#endif
