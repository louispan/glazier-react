{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Command where

import Control.Lens
import Control.Monad.State.Strict
import qualified GHCJS.Extras as E
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J

-- | Just change the state to something different so the React pureComponent will call render()
-- renderCmd :: Monad m => (sm -> [E.Property] -> J.JSVal -> cmd) -> G.GadgetT act sm m cmd
-- THe resulting command should be interpreted using 'componentSetState'
renderCmd :: MonadState sm m =>
           Lens' sm Int
           -- -> Getter sm Int
           -> Getter sm J.JSVal
           -> (sm -> [E.Property] -> J.JSVal -> cmd)
           -> m cmd
renderCmd frameNum componentRef fcmd = do
    frameNum %= (\i -> (i + 1) `mod` 100)
    i <- J.pToJSVal <$> use frameNum
    r <- use componentRef
    sm <- get
    pure $ fcmd sm [("frameNum", i)] r
