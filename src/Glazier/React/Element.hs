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
    , rawTextElement
    , mkCombinedElements
    ) where

import Control.DeepSeq
import Control.Newtype.Generics
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

-- | NB. No FromJS provided. See 'unsafeCoerceElement' below.
newtype ReactElement = ReactElement JE.JSRep
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString, NFData)

instance Newtype ReactElement

-- | Unfortunately, ReactJS did not export an easy way to check if something is a ReactElement,
-- although they do so in the internal code with REACT_ELEMENT_TYPE.
-- This function allow coercing a ReactElement from a JSVal
-- and is named unsafe as a reminder that the coersion is unchecked.
-- This function is required when receiving ReactElement from javascript (eg in a callback)
-- or to interface with foreign React Elements.
unsafeCoerceElement :: J.JSVal -> ReactElement
unsafeCoerceElement = ReactElement . JE.JSRep

-- | Create a react element (with children) from a HashMap of properties
mkBranchElement :: JE.JSRep -> [JE.Property] -> [ReactElement] -> IO ReactElement
mkBranchElement n props xs =
    js_mkBranchElement n (JE.propertiesToObject props) (JA.fromList $ JE.toJS <$> xs)

-- | Create a react element (with no children) from a HashMap of properties
mkLeafElement :: JE.JSRep -> [JE.Property] -> IO ReactElement
mkLeafElement n props =
    js_mkLeafElement n (JE.propertiesToObject props)

-- | Not an IO action because JSString is immutable
rawTextElement :: J.JSString -> ReactElement
rawTextElement = js_rawTextElement

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
    js_mkBranchElement :: JE.JSRep -> JO.Object -> JA.JSArray -> IO ReactElement

foreign import javascript unsafe
    "$r = hgr$React().createElement($1, $2);"
    js_mkLeafElement :: JE.JSRep -> JO.Object -> IO ReactElement

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
js_mkBranchElement :: JE.JSRep -> JO.Object -> JA.JSArray -> IO ReactElement
js_mkBranchElement _ _ _ = pure (ReactElement $ JE.JSRep J.nullRef)

js_mkLeafElement :: JE.JSRep -> JO.Object -> IO ReactElement
js_mkLeafElement _ _ =  pure (ReactElement $ JE.JSRep J.nullRef)

js_rawTextElement :: J.JSString -> ReactElement
js_rawTextElement _ = ReactElement $ JE.JSRep J.nullRef

js_mkCombinedElements :: JA.JSArray -> IO ReactElement
js_mkCombinedElements _ = pure (ReactElement $ JE.JSRep J.nullRef)

#endif
