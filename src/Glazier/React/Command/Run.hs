{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonomorphismRestriction #-}

-- | Common functions used by command interpreters
module Glazier.React.Command.Run where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified GHCJS.Types as J

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']({ frameNum : $1 }); }"
  js_setStateFrameNum :: Int -> J.JSVal -> IO ()

-- FIXME: What about wrapping large integers?
-- If the integer get large enought, javascript equality is true
reactSetState
    :: (MonadIO io, MonadState t io)
    => Setter' t sm
    -> Getter sm (MVar cm)
    -> Getter sm cm
    -> Setter' sm Int
    -> Getter sm Int
    -> Getter sm J.JSVal
    -> sm
    -> io ()
reactSetState superModel mModel cModel modelFrameNum modelFrameNum' modelRef sm = do
    -- increment the sequence number if render is required
    let sm' = sm & modelFrameNum %~ (+ 1)
        mm = sm' ^. mModel
        cm = sm' ^. cModel
    superModel .= sm'
    liftIO . void $ swapMVar mm cm -- ^ so that the render callback can use the latest state
    let i = sm' ^. modelFrameNum'
        r = sm' ^. modelRef
    liftIO $ js_setStateFrameNum i r -- ^ notify React that the specific component has changed
