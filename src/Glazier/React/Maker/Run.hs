module Glazier.React.Maker.Run
    ( run
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React.Component as R
import Glazier.React.Maker as R
import qualified Pipes.Concurrent as PC

mkActionCallback
    :: PC.Output act
    -> (J.JSVal -> MaybeT IO [act])
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            acts <- handler evt
            traverse_ (\act -> lift $ atomically $ PC.send output act >>= guard) acts

run :: PC.Output act -> R.Maker act (IO a) -> IO a
run output (R.MkHandler handler g) = mkActionCallback output handler >>= g

run _ (R.MkModelMVar g) = newEmptyMVar >>= g

run _ (R.MkRenderer ms render g) = J.syncCallback1' (R.onRender ms render') >>= g
  where
    render' v = hoist (hoist generalize) (render v)

run _ (R.PutModelMVar ms s g) = putMVar ms s >> g
