{-# LANGUAGE OverloadedStrings #-}

-- | Window here refers to 'Glazier.Window' (ie. rendering View of state), not browser window
module Glazier.React.Element
    ( ReactElement(..)
    , createElement
    , combineElements
    ) where

import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Types (IsJSVal, JSString, JSVal, jsval)
import JavaScript.Array (JSArray, fromList)

-- | The constructor for ReactElement is exported for convenient interoperabilithy with foreign
-- react components. It is assumed that it is only constructed for JSVal that have been created
-- using `Glazier.React.createElement` or equivalent.
-- Unfortunately, ReactJS did not export an easy way to check if something is a ReactElement,
-- although they do so in the internal code with REACT_ELEMENT_TYPE.
newtype ReactElement = ReactElement JSVal
instance IsJSVal ReactElement
instance PToJSVal ReactElement where
    pToJSVal = jsval

foreign import javascript unsafe
  "$r = React.createElement($1, $2, $3);"
  js_createElement :: JSString -> JSVal -> JSArray -> ReactElement

createElement :: JSString -> JSVal -> JSArray -> ReactElement
createElement = js_createElement

-- | React only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div'
combineElements :: JSVal -> [ReactElement] -> ReactElement
combineElements props xs = js_createElement "div" props (fromList $ jsval <$> xs)
