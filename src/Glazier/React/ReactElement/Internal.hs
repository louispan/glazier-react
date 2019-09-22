{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactElement.Internal
    ( ReactElement(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | https://reactjs.org/docs/react-api.html#creating-react-elements
newtype ReactElement = ReactElement J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance JE.FromJS ReactElement where
    validInstance = js_isReactElement
    fromJS a | js_isReactElement a = Just $ ReactElement a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "hgr$React().isValidElement($1)"
    js_isReactElement :: J.JSVal -> Bool

#else

js_isReactElement :: J.JSVal -> Bool
js_isReactElement _ = False

#endif
