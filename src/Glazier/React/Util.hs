module Glazier.React.Util where

import qualified Data.JSString as J
import qualified GHCJS.Types as J

-- | Creates a space delimited list of classnames for all tuple with true.
classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = J.jsval . J.unwords . fmap fst . filter snd
