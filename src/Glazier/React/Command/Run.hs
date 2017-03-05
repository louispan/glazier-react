{-# LANGUAGE RankNTypes #-}

-- | Common functions used by command interpreters
module Glazier.React.Command.Run
    ( componentSetState
    ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import qualified GHCJS.Extras as E
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Model.Class as R

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_componentSetState :: J.JSVal -> J.JSVal -> IO ()

componentSetState :: (R.HasCModel sm cm, R.HasMModel sm cm) => sm -> [E.Property] -> J.JSVal -> IO ()
componentSetState sm props j = do
    let cm = sm ^. R.cModel
        mm = sm ^. R.mModel
    void $ swapMVar mm cm
    dict <- E.toMaybeJSObject props
    let dict' = J.pToJSVal $ E.PureJSVal <$> dict
    js_componentSetState dict' j
