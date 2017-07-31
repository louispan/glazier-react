module Glazier.React.Maker.Exec where

import Control.Concurrent.STM.TMVar
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
onRender :: G.WindowT mdl (R.ReactMlT IO) () -> TMVar mdl -> IO J.JSVal
onRender render frm = do
    mdl <- atomically $ readTMVar frm
    JE.toJS <$> R.markedElement render mdl

mkActionCallback
    :: PC.Output act
    -> (J.JSVal -> MaybeT IO [act])
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            acts <- handler evt
            lift $ atomically $ traverse_ (\act -> PC.send output act >>= guard) acts

execMaker :: TMVar Int -> R.ReactComponent -> PC.Output act -> R.Maker act (IO a) -> IO a
execMaker _ _ output (R.MkHandler handler g) = mkActionCallback output handler >>= g

execMaker _ _ _ (R.MkEmptyFrame g) = atomically newEmptyTMVar >>= g

execMaker _ _ _ (R.MkRenderer render frm g) = J.syncCallback' (onRender render' frm) >>= g
  where
    render' = hoist (hoist generalize) render

execMaker _ _ _ (R.PutFrame frm mdl g) = atomically (putTMVar frm mdl) >> g

execMaker _ component _ (R.GetComponent g) = g component

execMaker muid _ _ (R.MkKey g) = do
    uid' <- atomically $ do
        -- expects that muid is not empty!
        uid <- readTMVar muid
        let uid' = (uid `mod` JE.maxSafeInteger) + 1
        void $ swapTMVar muid uid'
        pure uid'
    g (JS.pack . show $ uid')
