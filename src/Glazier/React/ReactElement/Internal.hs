{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.ReactElement.Internal
    ( ReactElement(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data

-- | https://reactjs.org/docs/react-api.html#creating-react-elements
newtype ReactElement = ReactElement JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS ReactElement where
    validFromJS = js_isReactElement
    fromJS a | js_isReactElement a = Just $ ReactElement a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = hgr$React().isValidElement($1);"
    js_isReactElement :: JSVal -> Bool

#else

js_isReactElement :: JSVal -> Bool
js_isReactElement _ = False

#endif
