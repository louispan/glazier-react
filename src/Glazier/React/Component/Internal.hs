{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Component.Internal
    ( Component(..)
    , ElementComponent(..)
    , elementComponent
    , WidgetComponent(..)
    , widgetComponent
    , WidgetRef(..)
    , rerenderWidget
    , batchWidgetRerender
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import Glazier.React.ReactBatch
import JS.Data


class ToJS j => Component j where
  componentName :: j -> JSString
  isStringComponent :: j -> Bool

-- The componentName is used for making the ReactPath for logging
instance Component JSString where
  componentName = id
  isStringComponent _ = True

instance Component ElementComponent where
  componentName _ = "element"
  isStringComponent _ = False

-- | Returns a reference to the javascript *class* definition of the react component
newtype ElementComponent = ElementComponent JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

-- | This returns the javascript class definition of WidgetComponent.
-- There is ever only one WidgetComponent class, so it is purely available
elementComponent :: ElementComponent
elementComponent = ElementComponent js_elementComponent

-- | Returns a reference to the javascript *class* definition of the react component
newtype WidgetComponent = WidgetComponent JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

-- | This returns the javascript class definition of WidgetComponent.
-- There is ever only one WidgetComponent class, so it is purely available
widgetComponent :: WidgetComponent
widgetComponent = WidgetComponent js_widgetComponent

-- | Rerenders an instance of a component created using WidgetComponent.
rerenderWidget :: WidgetRef -> IO ()
rerenderWidget = js_rerenderWidget

-- | batch rerendering of a WidgetRef
batchWidgetRerender :: ReactBatch -> WidgetRef -> IO ()
batchWidgetRerender b w = js_batchWidgetRerender b w

-- | This is used store the react "ref" to a javascript instance of a react Component.
newtype WidgetRef = WidgetRef JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS WidgetRef where
    fromJS a | js_isWidgetComponent a = Just $ WidgetRef a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hgr$ElementComponent();"
  js_elementComponent :: JSVal

foreign import javascript unsafe
  "$r = hgr$WidgetComponent();"
  js_widgetComponent :: JSVal

-- -- !!blah is javascript way of converting to bool
-- -- using undocumented api to check if something is react component
-- -- https://stackoverflow.com/questions/33199959/how-to-detect-a-react-component-vs-a-react-element
-- foreign import javascript unsafe
--   "!(!($1 && !(!($1['isReactComponent']))))"
--   js_isReactComponent :: JSVal -> Bool

foreign import javascript unsafe
    "$r = typeof $1 !== 'undefined' && $1 instanceof hgr$WidgetComponent;"
    js_isWidgetComponent :: JSVal -> Bool

foreign import javascript unsafe
    "$1.rerender();"
    js_rerenderWidget :: WidgetRef -> IO ()

foreign import javascript unsafe
    "$1.batch($2.rerender);"
    js_batchWidgetRerender :: ReactBatch -> WidgetRef -> IO ()

#else

js_elementComponent :: JSVal
js_elementComponent = nullRef

js_widgetComponent :: JSVal
js_widgetComponent = nullRef

js_isWidgetComponent :: JSVal -> Bool
js_isWidgetComponent _ = False

js_rerenderWidget :: WidgetRef -> IO ()
js_rerenderWidget _ = pure ()

js_batchWidgetRerender :: ReactBatch -> WidgetRef -> IO ()
js_batchWidgetRerender _ _ = pure ()

#endif
