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
  "if ($1 && $1['setState']) { $1['setState']({ frameNum : $2 }); }"
  js_setStateFrameNum :: J.JSVal -> Int -> IO ()

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
    liftIO $ js_setStateFrameNum r i -- ^ notify React that the specific component has changed
