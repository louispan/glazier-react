-- | Refers to 'Glazier.Window' (ie. rendering View of state), not browser window
module Glazier.React.Window where

import GHCJS.Nullable (Nullable(..))
import GHCJS.Types (JSString, JSVal, IsJSVal)
import JavaScript.Array (JSArray)

newtype ReactElement = ReactElement JSVal
instance IsJSVal ReactElement

-- FIXME: Does this belong here?
newtype ReactProps = ReactProps JSVal
instance IsJSVal ReactProps

foreign import javascript unsafe
  "$r = React.createElement($1, $2, $3);"
  js_createElement :: JSString -> Nullable ReactProps -> Nullable JSArray -> IO ReactElement
