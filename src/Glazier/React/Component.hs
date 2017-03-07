module Glazier.React.Component
    ( ReactComponent
    , mkComponent
    ) where

import qualified Control.Disposable as CD
import qualified GHCJS.Types as J
import qualified GHCJS.Marshal.Pure as J

-- | A newtype wrapper to give a noop disposable instance to React components
-- This allows generic deriving of model Adaptors.
newtype ReactComponent = ReactComponent J.JSVal

instance CD.Disposing ReactComponent where
    disposing _ = CD.DisposeNone

instance J.IsJSVal ReactComponent
instance J.PToJSVal ReactComponent where
    pToJSVal = J.jsval

mkComponent :: IO ReactComponent
mkComponent = ReactComponent <$> js_mkComponent

foreign import javascript unsafe
  "$r = hgr$mkClass();"
  js_mkComponent
      :: IO J.JSVal
