{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Glazier.React.Gadget where

import Control.Lens
import Control.Monad.Trans.ACont
import Control.Monad.Trans.AReader
import Control.Monad.Trans.AState.Strict
import qualified Data.DList as DL
import Glazier.React.Entity

type GadgetT cmd p s m = AReaderT (Entity p s) (AContT () (AStateT (DL.DList cmd) m))
type Gadget cmd p s = GadgetT cmd p s Identity

gadgetT ::
    (Entity p s
        -> (a -> AStateT (DL.DList cmd) m ())
        -> AStateT (DL.DList cmd) m ())
    -> GadgetT cmd p s m a
gadgetT f = areaderT (\r -> acontT (f r))

runGadgetT ::
    GadgetT cmd p s m a
    -> Entity p s
    -> (a -> AStateT (DL.DList cmd) m ())
    -> AStateT (DL.DList cmd) m ()
runGadgetT x l = runAContT (runAReaderT x l)

evalGadgetT :: Monad m => GadgetT cmd p s m () -> Entity p s -> AStateT (DL.DList cmd) m ()
evalGadgetT gad ent = evalAContT . (`runAReaderT` ent) $ gad
