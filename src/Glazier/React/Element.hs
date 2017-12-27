{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Element
    ( ReactElement -- constructor is not exported
    , unsafeCoerceElement
    , mkBranchElement
    , mkLeafElement
    , textElement
    , mkCombinedElements
    ) where

import Control.DeepSeq
import qualified Control.Disposable as CD
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Array as JA
import qualified JavaScript.Object as JO
import qualified JavaScript.Extras as JE

-- | NB. No FromJS provided. See 'unsafeCoerceElement' below.
newtype ReactElement = ReactElement JE.JSVar
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance CD.Dispose ReactElement where
    dispose _ = mempty

-- | Unfortunately, ReactJS did not export an easy way to check if something is a ReactElement,
-- although they do so in the internal code with REACT_ELEMENT_TYPE.
-- This function allow coercing a ReactElement from a JSVal
-- and is named unsafe as a reminder that the coersion is unchecked.
-- This function is required when receiving ReactElement from javascript (eg in a callback)
-- or to interface with foreign React Elements.
unsafeCoerceElement :: J.JSVal -> ReactElement
unsafeCoerceElement = ReactElement . JE.JSVar

-- | Create a react element (with children) from a HashMap of properties
mkBranchElement :: JE.JSVar -> [JE.Property] -> [ReactElement] -> IO ReactElement
mkBranchElement n props xs =
    js_mkBranchElement n (JE.fromProperties props) (JA.fromList $ JE.toJS <$> xs)

-- | Create a react element (with no children) from a HashMap of properties
mkLeafElement :: JE.JSVar -> [JE.Property] -> IO ReactElement
mkLeafElement n props =
    js_mkLeafElement n (JE.fromProperties props)

-- | Not an IO action because JSString is immutable
textElement :: J.JSString -> ReactElement
textElement = js_textElement

-- | React only allows a single top most element.
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
    js_mkBranchElement :: JE.JSVar -> JO.Object -> JA.JSArray -> IO ReactElement

foreign import javascript unsafe
    "$r = hgr$React().createElement($1, $2);"
    js_mkLeafElement :: JE.JSVar -> JO.Object -> IO ReactElement

foreign import javascript unsafe
    "$r = $1;"
    js_textElement :: J.JSString -> ReactElement

-- | Wrap a list of ReactElements inside a 'div'
foreign import javascript unsafe
    "$r = hgr$mkCombinedElements($1);"
    js_mkCombinedElements :: JA.JSArray -> IO ReactElement

#else

-- | This is an IO action because even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
js_mkBranchElement :: JE.JSVar -> JO.Object -> JA.JSArray -> IO ReactElement
js_mkBranchElement _ _ _ = pure (ReactElement $ JE.JSVar J.nullRef)

js_mkLeafElement :: JE.JSVar -> JO.Object -> IO ReactElement
js_mkLeafElement _ _ =  pure (ReactElement $ JE.JSVar J.nullRef)

js_textElement :: J.JSString -> ReactElement
js_textElement _ = ReactElement $ JE.JSVar J.nullRef

js_mkCombinedElements :: JA.JSArray -> IO ReactElement
js_mkCombinedElements _ = pure (ReactElement $ JE.JSVar J.nullRef)

#endif
