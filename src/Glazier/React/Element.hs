{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Element
    ( ReactElement
    , unsafeCoerceElement
    , mkBranchElement
    , mkLeafElement
    , textElement
    , mkCombinedElements
    ) where

import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

newtype ReactElement = ReactElement J.JSVal
instance J.IsJSVal ReactElement
instance J.PToJSVal ReactElement where
    pToJSVal = J.jsval

-- | Unfortunately, ReactJS did not export an easy way to check if something is a ReactElement,
-- although they do so in the internal code with REACT_ELEMENT_TYPE.
-- This function allow coercing a ReactElement from a JSVal
-- and is marked unsafe as a reminder that the coersion is unchecked.
-- This function is required when receiving ReactElement from javascript (eg in a callback)
-- or to interface with foreign React Elements.
unsafeCoerceElement :: J.JSVal -> ReactElement
unsafeCoerceElement = ReactElement

-- | Create a react element (with children) from a HashMap of properties
mkBranchElement :: J.JSVal -> [JE.Property] -> [ReactElement] -> IO ReactElement
mkBranchElement n props xs = do
    props' <- JE.toMaybeJSObject props
    js_mkBranchElement n (J.pToJSVal (JE.PureJSVal <$> props')) (JA.fromList $ J.jsval <$> xs)

-- | Create a react element (with no children) from a HashMap of properties
mkLeafElement :: J.JSVal -> [JE.Property] -> IO ReactElement
mkLeafElement n props = do
    props' <- JE.toMaybeJSObject props
    js_mkLeafElement n (J.pToJSVal (JE.PureJSVal <$> props'))

-- | Not an IO action because JSString is immutable
textElement :: J.JSString -> ReactElement
textElement = js_textElement

-- | React only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div' if required.
-- If there is only one element in the list, then nothing is changed.
mkCombinedElements :: [ReactElement] -> IO ReactElement
mkCombinedElements xs = js_mkCombinedElements (JA.fromList $ J.jsval <$> xs)

#ifdef __GHCJS__

-- | This is an IO action because even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
foreign import javascript unsafe
    "$r = React.createElement($1, $2, $3);"
    js_mkBranchElement :: J.JSVal -> J.JSVal -> JA.JSArray -> IO ReactElement

foreign import javascript unsafe
    "$r = React.createElement($1, $2);"
    js_mkLeafElement :: J.JSVal -> J.JSVal -> IO ReactElement

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
js_mkBranchElement :: J.JSVal -> J.JSVal -> JA.JSArray -> IO ReactElement
js_mkBranchElement _ _ _ = pure (ReactElement J.nullRef)

js_mkLeafElement :: J.JSVal -> J.JSVal -> IO ReactElement
js_mkLeafElement _ _ =  pure (ReactElement J.nullRef)

js_textElement :: J.JSString -> ReactElement
js_textElement _ = ReactElement J.nullRef

js_mkCombinedElements :: JA.JSArray -> IO ReactElement
js_mkCombinedElements _ = pure (ReactElement J.nullRef)

#endif
