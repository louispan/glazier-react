-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Control.Monad.State.Strict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.Obj

-- | A 'Gadget' is an instance of 'MonadReactor'
-- The @s@ state can be magnified with 'magnifiedEntity'
type Gadget c o s = ReaderT (Entity o s) (ContT () (StateT J.JSString (Program c)))

toGadget ::
    (Entity o s
        -> (a -> StateT J.JSString (Program c) ())
        -> StateT J.JSString (Program c) ())
    -> Gadget c o s a
toGadget f = ReaderT (\r -> ContT (f r))

runGadget ::
    Gadget c o s a
    -> Entity o s
    -> (a -> StateT J.JSString (Program c) ())
    -> StateT J.JSString (Program c) ()
runGadget x l = runContT (runReaderT x l)

gadgetWith :: WeakObj s -> Gadget c s s a -> ContT () (StateT J.JSString (Program c)) a
gadgetWith obj = (`runReaderT` (Entity obj id))

gadgetWith' :: Obj s -> Gadget c s s a -> ContT () (StateT J.JSString (Program c)) a
gadgetWith' obj = gadgetWith (weakObj obj)

evalGadget :: Gadget c o s () -> Entity o s -> StateT J.JSString (Program c) ()
evalGadget gad ent = evalContT . (`runReaderT` ent) $ gad
