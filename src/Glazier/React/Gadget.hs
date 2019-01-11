-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.Obj

-- | A 'Gadget' is an instance of 'MonadGadget'
-- The @s@ state can be magnified with 'magnifiedEntity'
type Gadget c o s = ReaderT (Entity o s) (ContT () (Program c))

toGadget ::
    (Entity o s
        -> (a -> Program c ())
        -> Program c ())
    -> Gadget c o s a
toGadget f = ReaderT (\r -> ContT (f r))

runGadget ::
    Gadget c o s a
    -> Entity o s
    -> (a -> Program c ())
    -> Program c ()
runGadget x l = runContT (runReaderT x l)

gadgetWith :: GetWeakObj o s => o -> Gadget c s s a -> ContT () (Program c) a
gadgetWith obj = (`runReaderT` (Entity id (weakObj obj)))

evalGadget :: Gadget c o s () -> Entity o s -> (Program c) ()
evalGadget gad ent = evalContT . (`runReaderT` ent) $ gad
