module Glazier.React.Reactor.Run where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.Gizmo as G
import qualified Glazier.React.Component as R
import Glazier.React.Reactor as R
import qualified Glazier.React.Markup as R
import JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC

-- | This is called synchronously by React to render the DOM.
-- This must not block!
onRender :: G.GizmoT s (R.ReactMlT IO) () -> s -> IO J.JSVal
onRender render s = JE.toJS <$> R.markedElement render s

mkActionCallback
    :: PC.Output act
    -> (J.JSVal -> MaybeT IO [act])
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            acts <- handler evt
            lift $ atomically $ traverse_ (\act -> PC.send output act >>= guard) acts

runReactor :: TMVar Int -> R.ReactComponent -> R.Reactor (IO a) -> IO a
runReactor _ _ (R.MkHandler out handler g) = mkActionCallback out handler >>= g

runReactor _ _ (R.MkRenderer render s g) = J.syncCallback' (onRender render' s) >>= g
  where
    render' = hoist (hoist atomically) render

runReactor _ component (R.GetComponent g) = g component

runReactor muid _ (R.MkKey g) = atomically go >>= g
  where
    go = do
        -- expects that muid is not empty!
        uid <- readTMVar muid
        let uid' = (uid `mod` JE.maxSafeInteger) + 1
        void $ swapTMVar muid uid'
        pure uid'

-- runReactor _ _ (R.DoNewTVar s g) = newTVarIO s >>= g

-- runReactor _ _ (R.DoModifyTVar v h x) = atomically (modifyTVar' v h) >> x

runReactor _ _ (R.DoNewEmptyTMVar g) = newEmptyTMVarIO >>= g

runReactor _ _ (R.DoPutTMVar v a x) = atomically (putTMVar v a) >> x

runReactor _ _ (R.SendAction o a x) = atomically (PC.send o a) >> x
