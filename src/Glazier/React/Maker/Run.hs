module Glazier.React.Maker.Run where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React.Component as R
import Glazier.React.Maker as R
import qualified Pipes.Concurrent as PC

mkActionCallback
    :: PC.Output act
    -> (J.JSVal -> MaybeT IO act)
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            action <- handler evt
            lift $ void $ atomically $ PC.send output action

runMaker :: PC.Output act -> R.Maker act (IO a) -> IO a
runMaker output (R.MkHandler handler g) = mkActionCallback output handler >>= g

runMaker _ (R.MkModelMVar g) = newEmptyMVar >>= g

runMaker _ (R.MkRenderer ms render g) = J.syncCallback1' (R.onRender ms render') >>= g
  where
    render' v = hoist (hoist generalize) (render v)

runMaker _ (R.PutModelMVar ms s g) = putMVar ms s >> g
