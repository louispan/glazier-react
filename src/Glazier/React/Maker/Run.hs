module Glazier.React.Maker.Run where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import Glazier.React.Maker as R
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R
import qualified Pipes.Concurrent as PC

-- | This is called synchronously by React to render the DOM.
-- This must not block!
onRender :: MVar s -> (J.JSVal -> G.WindowT s (R.ReactMlT IO) ()) -> J.JSVal -> IO J.JSVal
onRender mm wind v = do
    mdl <- readMVar mm
    J.pToJSVal <$> R.markedElement (wind v) mdl

mkActionCallback
    :: PC.Output act
    -> (J.JSVal -> MaybeT IO [act])
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            acts <- handler evt
            traverse_ (\act -> lift $ atomically $ PC.send output act >>= guard) acts

run :: R.ReactComponent -> PC.Output act -> R.Maker act (IO a) -> IO a
run _ output (R.MkHandler handler g) = mkActionCallback output handler >>= g

run _ _ (R.MkEmptyMModel g) = newEmptyMVar >>= g

run _ _ (R.MkRenderer ms render g) = J.syncCallback1' (onRender ms render') >>= g
  where
    render' v = hoist (hoist generalize) (render v)

run _ _ (R.PutMModel ms s g) = putMVar ms s >> g

run component _ (R.GetComponent g) = g component
