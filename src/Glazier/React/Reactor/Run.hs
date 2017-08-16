module Glazier.React.Reactor.Run where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.Morph
import qualified GHCJS.Foreign.Callback as J
import qualified Glazier.React.Component as R
import Glazier.React.Reactor as R
import qualified Glazier.React.Markup as R
import JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC

runReactor :: TMVar Int -> R.ReactComponent -> R.Reactor (IO a) -> IO a
runReactor _ _ (R.MkHandler h g) = J.syncCallback1 J.ContinueAsync h >>= g

runReactor _ _ (R.MkRenderer rnd g) = J.syncCallback' rnd'' >>= g
  where
    rnd' = hoist atomically rnd
    -- | This is called synchronously by React to render the DOM.
    -- This must not block!
    rnd'' = JE.toJS <$> R.toElement rnd'

runReactor _ component (R.GetComponent g) = g component

runReactor muid _ (R.MkKey g) = atomically go >>= g
  where
    go = do
        -- expects that muid is not empty!
        uid <- readTMVar muid
        let uid' = (uid `mod` JE.maxSafeInteger) + 1
        void $ swapTMVar muid uid'
        pure uid'

runReactor _ _ (R.DoSpawn b g) = PC.spawn b >>= g

runReactor _ _ (R.DoSTM m g) = atomically m >>= g

execReactor :: TMVar Int -> R.ReactComponent -> F R.Reactor a -> IO a
execReactor muid comp = iterM (runReactor muid comp)
