{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Component.Internal
    ( ReactComponent(..) -- constructor exported
    , reactComponent
    , ReactComponentRef(..) -- constructor exported
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | Returns a reference to the javascript class definition
-- of the shim wrapper around ReactPureComponent
newtype ReactComponent = ReactComponent JE.JSRep
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

-- | There is ever only one shim class, so it is purely available
reactComponent :: ReactComponent
reactComponent = ReactComponent js_reactComponent

-- | This is used store the react "ref" to a javascript instance
-- of the ReactComponent, so that react "this.setState" can be called.
newtype ReactComponentRef = ReactComponentRef JE.JSRep
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$component();"
  js_reactComponent
      :: JE.JSRep

#else

js_reactComponent :: JE.JSRep
js_reactComponent = JE.JSRep J.nullRef

#endif
