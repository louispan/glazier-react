{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Command
    ( basicRenderCmd
    ) where

import Control.Lens
import Control.Monad.State.Strict
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

-- | Just change the state to something different so the React pureComponent will call render()
-- renderCmd :: Monad m => (sm -> [JE.Property] -> J.JSVal -> cmd) -> G.GadgetT act sm m cmd
-- The resulting command should be interpreted using 'componentSetState'
basicRenderCmd :: MonadState sm m =>
           Lens' sm Int
           -- -> Getter sm Int
           -> Getter sm J.JSVal
           -> (sm -> [JE.Property] -> J.JSVal -> cmd)
           -> m cmd
basicRenderCmd frameNum componentRef fcmd = do
    frameNum %= (\i -> (i `mod` js_maxSafeInt) + 1)
    i <- J.pToJSVal <$> use frameNum
    r <- use componentRef
    sm <- get
    pure $ fcmd sm [("frameNum", i)] r

foreign import javascript unsafe
  "Number.MAX_SAFE_INTEGER"
  js_maxSafeInt :: Int
