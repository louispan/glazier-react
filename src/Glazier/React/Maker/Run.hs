module Glazier.React.Maker.Run where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified Data.JSString as JS
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC

-- | This is called synchronously by React to render the DOM.
-- This must not block!
onRender :: G.WindowT mdl (R.ReactMlT IO) () -> MVar mdl -> IO J.JSVal
onRender render frm = do
    mdl <- readMVar frm
    JE.toJS <$> R.markedElement render mdl

mkActionCallback
    :: PC.Output act
    -> (J.JSVal -> MaybeT IO [act])
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            acts <- handler evt
            traverse_ (\act -> lift $ atomically $ PC.send output act >>= guard) acts

run :: MVar Int -> R.ReactComponent -> PC.Output act -> R.Maker act (IO a) -> IO a
run _ _ output (R.MkHandler handler g) = mkActionCallback output handler >>= g

run _ _ _ (R.MkEmptyFrame g) = newEmptyMVar >>= g

run _ _ _ (R.MkRenderer render frm g) = J.syncCallback' (onRender render' frm) >>= g
  where
    render' = hoist (hoist generalize) render

run _ _ _ (R.PutFrame frm mdl g) = putMVar frm mdl >> g

run _ component _ (R.GetComponent g) = g component

run muid _ _ (R.MkKey g) = do
    -- expects that muid is not empty!
    uid <- readMVar muid
    let uid' = (uid `mod` JE.maxSafeInteger) + 1
    void $ swapMVar muid uid'
    g (JS.pack . show $ uid')
