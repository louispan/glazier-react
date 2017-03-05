{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonomorphismRestriction #-}

-- | Common functions used by command interpreters
module Glazier.React.Command.Run
    ( componentSetState
    ) where

import Control.Monad
import Control.Concurrent.MVar
import qualified GHCJS.Extras as E
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_componentSetState :: J.JSVal -> J.JSVal -> IO ()

componentSetState :: (MVar s, s) -> [E.Property] -> J.JSVal -> IO ()
componentSetState (mm, cm) props j = do
    void $ swapMVar mm cm
    dict <- E.toMaybeJSObject props
    let dict' = J.pToJSVal $ E.PureJSVal <$> dict
    js_componentSetState dict' j
