-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Glazier.React.Entity
import Glazier.React.Obj
import Glazier.Command

-- | The @s@ state can be magnified with 'magnifiedEntity'
type Gadget cmd p s = ReaderT (Entity p s) (ContT () (Program cmd))

toGadget ::
    (Entity p s
        -> (a -> Program cmd ())
        -> Program cmd ())
    -> Gadget cmd p s a
toGadget f = ReaderT (\r -> ContT (f r))

runGadget ::
    Gadget cmd p s a
    -> Entity p s
    -> (a -> Program cmd ())
    -> Program cmd ()
runGadget x l = runContT (runReaderT x l)

gadgetWith :: WeakObj s -> Gadget cmd s s a -> ContT () (Program cmd) a
gadgetWith obj = (`runReaderT` (Entity obj id))

gadgetWith' :: Obj s -> Gadget cmd s s a -> ContT () (Program cmd) a
gadgetWith' obj = gadgetWith (weakObj obj)

evalGadget :: Gadget cmd p s () -> Entity p s -> Program cmd ()
evalGadget gad ent = evalContT . (`runReaderT` ent) $ gad
