{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Element
    ( ReactElement
    , unsafeCoerceReactElement
    , createReactElement
    , textReactElement
    , combineReactElements
    , Properties
    ) where

import qualified Data.HashMap.Strict as M
import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Types (IsJSVal, JSString, JSVal, jsval)
import Glazier.React.Internal (PureJSVal(..))
import JavaScript.Array (JSArray, fromList)
import JavaScript.Object (Object, create, unsafeSetProp)

newtype ReactElement = ReactElement JSVal
instance IsJSVal ReactElement
instance PToJSVal ReactElement where
    pToJSVal = jsval

-- | This is an IO action even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
foreign import javascript unsafe
    "$r = React.createElement($1, $2, $3);"
    js_createReactElement :: JSString -> JSVal -> JSArray -> IO ReactElement

-- | Unfortunately, ReactJS did not export an easy way to check if something is a ReactElement,
-- although they do so in the internal code with REACT_ELEMENT_TYPE.
-- This function allow coercing a ReactElement from a JSVal
-- and is marked unsafe as a reminder that the coersion is unchecked.
-- This function is required when receiving ReactElement from javascript (eg in a callback)
-- or to interface with foreign React Elements.
unsafeCoerceReactElement :: JSVal -> ReactElement
unsafeCoerceReactElement = ReactElement

-- | Create a JS Object from a HashMap
-- TODO: Is is possible/more efficient to do this inline? eg {a: 1, b: 2}
toJSProps :: M.HashMap JSString JSVal -> IO (Maybe Object)
toJSProps m | M.null m = pure Nothing
toJSProps m | otherwise = do
                    obj <- create
                    M.foldlWithKey' (\action k v -> action >> unsafeSetProp k v obj) (pure ()) m
                    pure (Just obj)

type Properties = M.HashMap JSString JSVal

-- | Create a react from a HashMap of properties
createReactElement :: JSString -> Properties -> [ReactElement] -> IO ReactElement
createReactElement n props xs = do
    props' <- toJSProps props
    js_createReactElement n (pToJSVal (PureJSVal <$> props')) (fromList $ jsval <$> xs)

foreign import javascript unsafe
    "$r = $1;"
    js_textReactElement :: JSString -> ReactElement

-- | Not an IO action because JSString is immutable
textReactElement :: JSString -> ReactElement
textReactElement = js_textReactElement

-- | Wrap a list of ReactElements inside a 'div'
foreign import javascript unsafe
    "$r = hgr$combineElements($1);"
    js_combineElements :: JSArray -> IO ReactElement

-- | React only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div' if required.
-- If there is only one element in the list, then nothing is changed.
combineReactElements :: [ReactElement] -> IO ReactElement
combineReactElements xs = js_combineElements (fromList $ jsval <$> xs)
