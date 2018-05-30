-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Control.Monad.Trans.ACont
import Control.Monad.Trans.AReader
import Control.Monad.Trans.AState.Strict
import qualified Data.DList as DL
import Glazier.React.Entity
import Glazier.React.Subject

type Gadget cmd p s = AReaderT (Entity p s) (AContT () (AState (DL.DList cmd)))

toGadget ::
    (Entity p s
        -> (a -> AState (DL.DList cmd) ())
        -> AState (DL.DList cmd) ())
    -> Gadget cmd p s a
toGadget f = areaderT (\r -> acontT (f r))

runGadget ::
    Gadget cmd p s a
    -> Entity p s
    -> (a -> AState (DL.DList cmd) ())
    -> AState (DL.DList cmd) ()
runGadget x l = runAContT (runAReaderT x l)

gadgetWith :: Subject s -> Gadget cmd s s a -> AContT () (AState (DL.DList cmd)) a
gadgetWith sbj = (`runAReaderT` (Entity sbj id))

evalGadget :: Gadget cmd p s () -> Entity p s -> AState (DL.DList cmd) ()
evalGadget gad ent = evalAContT . (`runAReaderT` ent) $ gad
