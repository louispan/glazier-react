-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.DList as DL
import Glazier.React.Entity
import Glazier.React.Subject

-- | The @s@ state can be magnified with 'magnifiedEntity'
type Gadget cmd p s = ReaderT (Entity p s) (ContT () (State (DL.DList cmd)))

toGadget ::
    (Entity p s
        -> (a -> State (DL.DList cmd) ())
        -> State (DL.DList cmd) ())
    -> Gadget cmd p s a
toGadget f = ReaderT (\r -> ContT (f r))

runGadget ::
    Gadget cmd p s a
    -> Entity p s
    -> (a -> State (DL.DList cmd) ())
    -> State (DL.DList cmd) ()
runGadget x l = runContT (runReaderT x l)

gadgetWith :: Subject s -> Gadget cmd s s a -> ContT () (State (DL.DList cmd)) a
gadgetWith sbj = (`runReaderT` (Entity sbj id))

evalGadget :: Gadget cmd p s () -> Entity p s -> State (DL.DList cmd) ()
evalGadget gad ent = evalContT . (`runReaderT` ent) $ gad
