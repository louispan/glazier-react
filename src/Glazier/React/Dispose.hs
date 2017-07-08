{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Dispose where

import Data.Foldable
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras.JSVar as JE

-- | A 'Dispose' is something with some resources to release
class Dispose a where
    dispose :: a -> IO ()
    default dispose :: (G.Generic a, GDispose (G.Rep a)) => a -> IO ()
    dispose x = gDispose $ G.from x

-- | Generic instance basically traverses the data type structure
-- and expects the values to be all instances of 'Dispose'
class GDispose f where
    gDispose :: f p -> IO ()

instance GDispose G.U1 where
  gDispose G.U1 = pure ()

instance (GDispose f, GDispose g) => GDispose (f G.:+: g) where
  gDispose (G.L1 x) = gDispose x
  gDispose (G.R1 x) = gDispose x

instance (GDispose f, GDispose g) => GDispose (f G.:*: g) where
  gDispose (x G.:*: y) = gDispose x >> gDispose y

instance (Dispose c) => GDispose (G.K1 i c) where
  gDispose (G.K1 x) = dispose x

instance (GDispose f) => GDispose (G.M1 i t f) where
  gDispose (G.M1 x) = gDispose x

------------------------------

instance Dispose (J.Callback a) where
    dispose = J.releaseCallback

instance Dispose J.JSString where
    dispose _ = pure ()

instance Dispose JE.JSVar where
    dispose _ = pure ()

instance (Dispose a, Dispose b) => Dispose (a, b) where
    dispose (a, b) = dispose a >> dispose b

instance (Dispose a, Dispose b, Dispose c) => Dispose (a, b, c) where
    dispose (a, b, c) = dispose a >> dispose b >> dispose c

instance Dispose a => Dispose [a] where
    dispose = traverse_ dispose

instance Dispose Int where
    dispose _ = pure ()

instance Dispose J.JSVal where
    dispose _ = pure ()
