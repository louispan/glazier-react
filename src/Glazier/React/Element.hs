{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Element
    ( ReactElement
    , unsafeCoerceElement
    , mkBranchElement
    , mkLeafElement
    , textElement
    , mkCombinedElements
    , Properties
    ) where

import qualified Data.HashMap.Strict as M
import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Types (IsJSVal, JSString, JSVal, jsval)
import Glazier.React.Internal (PureJSVal(..))
import JavaScript.Array (JSArray, fromList)
import JavaScript.Object (Object, create, unsafeSetProp)
import Data.JSString.Text (textToJSString)
import qualified Data.Text as T

newtype ReactElement = ReactElement JSVal
instance IsJSVal ReactElement
instance PToJSVal ReactElement where
    pToJSVal = jsval

-- | This is an IO action even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
foreign import javascript unsafe
    "$r = React.createElement($1, $2, $3);"
    js_mkBranchElement :: JSString -> JSVal -> JSArray -> IO ReactElement

foreign import javascript unsafe
    "$r = React.createElement($1, $2);"
    js_mkLeafElement :: JSVal -> JSVal -> IO ReactElement


-- | Unfortunately, ReactJS did not export an easy way to check if something is a ReactElement,
-- although they do so in the internal code with REACT_ELEMENT_TYPE.
-- This function allow coercing a ReactElement from a JSVal
-- and is marked unsafe as a reminder that the coersion is unchecked.
-- This function is required when receiving ReactElement from javascript (eg in a callback)
-- or to interface with foreign React Elements.
unsafeCoerceElement :: JSVal -> ReactElement
unsafeCoerceElement = ReactElement

type Properties = M.HashMap T.Text JSVal

-- | Create a JS Object from a HashMap
toJSProps :: Properties -> IO (Maybe Object)
toJSProps m | M.null m = pure Nothing
toJSProps m | otherwise = do
                    obj <- create
                    M.foldlWithKey' (\action k v -> action >> unsafeSetProp (textToJSString k) v obj) (pure ()) m
                    pure (Just obj)

-- | Create a react element (with children) from a HashMap of properties
mkBranchElement :: JSString -> Properties -> [ReactElement] -> IO ReactElement
mkBranchElement n props xs = do
    props' <- toJSProps props
    js_mkBranchElement n (pToJSVal (PureJSVal <$> props')) (fromList $ jsval <$> xs)

-- | Create a react element (with no children) from a HashMap of properties
mkLeafElement :: JSVal -> Properties -> IO ReactElement
mkLeafElement n props = do
    props' <- toJSProps props
    js_mkLeafElement n (pToJSVal (PureJSVal <$> props'))

foreign import javascript unsafe
    "$r = $1;"
    js_textElement :: JSString -> ReactElement

-- | Not an IO action because JSString is immutable
textElement :: JSString -> ReactElement
textElement = js_textElement

-- | Wrap a list of ReactElements inside a 'div'
foreign import javascript unsafe
    "$r = hgr$mkCombinedElements($1);"
    js_mkCombinedElements :: JSArray -> IO ReactElement

-- | React only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div' if required.
-- If there is only one element in the list, then nothing is changed.
mkCombinedElements :: [ReactElement] -> IO ReactElement
mkCombinedElements xs = js_mkCombinedElements (fromList $ jsval <$> xs)
