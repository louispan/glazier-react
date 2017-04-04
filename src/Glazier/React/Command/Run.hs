{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- | Common functions used by command interpreters
module Glazier.React.Command.Run
    ( componentSetState
    ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import qualified GHCJS.Types as J
import qualified Glazier.React.Model as R
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

componentSetState :: R.HasGizmo giz mdl pln => giz -> [JE.Property] -> J.JSVal -> IO ()
componentSetState giz props j = do
    let scn = giz ^. R.scene
        frm = giz ^. R.frame
    void $ swapMVar frm scn
    js_componentSetState (JE.fromProperties props) j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_componentSetState :: JO.Object -> J.JSVal -> IO ()

#else

js_componentSetState :: JO.Object -> J.JSVal -> IO ()
js_componentSetState _ _ = pure ()

#endif
