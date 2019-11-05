{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.ReactElement
    ( ReactElement -- constructor is not exported
    , mkBranchElement
    , mkLeafElement
    , rawTextElement
    , mkCombinedElements
    ) where

import Glazier.React.ReactElement.Internal
import qualified JavaScript.Array as JA
import JS.Data

-- | Create a react element (with children) from a HashMap of properties
mkBranchElement :: JSVal -> [(JSString, JSVal)] -> [ReactElement] -> IO ReactElement
mkBranchElement n props xs = do
    o <- object_fromEntries (f <$> props)
    js_mkBranchElement n o (JA.fromList $ toJS <$> xs)
  where
    f (a, b) = (toJS a, b)

-- | Create a react element (with no children) from a HashMap of properties
mkLeafElement :: JSVal -> [(JSString, JSVal)] -> IO ReactElement
mkLeafElement n props = do
    o <- object_fromEntries (f <$> props)
    js_mkLeafElement n o
  where
    f (a, b) = (toJS a, b)

-- | Not an IO action because JSString is immutable
rawTextElement :: JSString -> ReactElement
rawTextElement = js_rawTextElement

-- | 'Glazier.React.ReactDOM.renderDOM' only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div' if required.
-- If there is only one element in the list, then nothing is changed.
mkCombinedElements :: [ReactElement] -> IO ReactElement
mkCombinedElements xs = js_mkCombinedElements (JA.fromList $ toJS <$> xs)

#ifdef __GHCJS__

-- | This is an IO action because even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
foreign import javascript unsafe
    "hgr$React().createElement($1, $2, $3);"
    js_mkBranchElement :: JSVal -> JSObject -> JA.JSArray -> IO ReactElement

foreign import javascript unsafe
    "hgr$React().createElement($1, $2);"
    js_mkLeafElement :: JSVal -> JSObject -> IO ReactElement

foreign import javascript unsafe
    "$1"
    js_rawTextElement :: JSString -> ReactElement

-- | Wrap a list of ReactElements inside a 'div'
foreign import javascript unsafe
    "hgr$mkCombinedElements($1);"
    js_mkCombinedElements :: JA.JSArray -> IO ReactElement

#else

-- | This is an IO action because even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
js_mkBranchElement :: JSVal -> JSObject -> JA.JSArray -> IO ReactElement
js_mkBranchElement _ _ _ = pure (ReactElement nullRef)

js_mkLeafElement :: JSVal -> JSObject -> IO ReactElement
js_mkLeafElement _ _ =  pure (ReactElement nullRef)

js_rawTextElement :: JSString -> ReactElement
js_rawTextElement _ = ReactElement nullRef

js_mkCombinedElements :: JA.JSArray -> IO ReactElement
js_mkCombinedElements _ = pure (ReactElement nullRef)

#endif
