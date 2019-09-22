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

import qualified GHCJS.Types as J
import Glazier.React.ReactElement.Internal
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

-- | Create a react element (with children) from a HashMap of properties
mkBranchElement :: J.JSVal -> [(J.JSString, J.JSVal)] -> [ReactElement] -> IO ReactElement
mkBranchElement n props xs =
    js_mkBranchElement n (JE.propertiesToObject props) (JA.fromList $ JE.toJS <$> xs)

-- | Create a react element (with no children) from a HashMap of properties
mkLeafElement :: J.JSVal -> [(J.JSString, J.JSVal)] -> IO ReactElement
mkLeafElement n props =
    js_mkLeafElement n (JE.propertiesToObject props)

-- | Not an IO action because JSString is immutable
rawTextElement :: J.JSString -> ReactElement
rawTextElement = js_rawTextElement

-- | 'Glazier.React.ReactDOM.renderDOM' only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div' if required.
-- If there is only one element in the list, then nothing is changed.
mkCombinedElements :: [ReactElement] -> IO ReactElement
mkCombinedElements xs = js_mkCombinedElements (JA.fromList $ JE.toJS <$> xs)

#ifdef __GHCJS__

-- | This is an IO action because even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
foreign import javascript unsafe
    "$r = hgr$React().createElement($1, $2, $3);"
    js_mkBranchElement :: J.JSVal -> JO.Object -> JA.JSArray -> IO ReactElement

foreign import javascript unsafe
    "$r = hgr$React().createElement($1, $2);"
    js_mkLeafElement :: J.JSVal -> JO.Object -> IO ReactElement

foreign import javascript unsafe
    "$r = $1;"
    js_rawTextElement :: J.JSString -> ReactElement

-- | Wrap a list of ReactElements inside a 'div'
foreign import javascript unsafe
    "$r = hgr$mkCombinedElements($1);"
    js_mkCombinedElements :: JA.JSArray -> IO ReactElement

#else

-- | This is an IO action because even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
js_mkBranchElement :: J.JSVal -> JO.Object -> JA.JSArray -> IO ReactElement
js_mkBranchElement _ _ _ = pure (ReactElement J.nullRef)

js_mkLeafElement :: J.JSVal -> JO.Object -> IO ReactElement
js_mkLeafElement _ _ =  pure (ReactElement J.nullRef)

js_rawTextElement :: J.JSString -> ReactElement
js_rawTextElement _ = ReactElement J.nullRef

js_mkCombinedElements :: JA.JSArray -> IO ReactElement
js_mkCombinedElements _ = pure (ReactElement J.nullRef)

#endif
