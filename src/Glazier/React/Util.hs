module Glazier.React.Util where

import qualified GHCJS.Types as J

-- | This makes it easier to use OverloadedStrings with inputs that accept a JSVal that could be a JSString
strval :: J.JSString -> J.JSVal
strval = J.jsval
