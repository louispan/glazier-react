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

-- | The @s@ state can be magnified with 'magnifiedEntity'
type Gadget cmd p s = ReaderT (Entity p s) (ContT () (StateT J.JSString (Program cmd)))

toGadget ::
    (Entity p s
        -> (a -> StateT J.JSString (Program cmd) ())
        -> StateT J.JSString (Program cmd) ())
    -> Gadget cmd p s a
toGadget f = ReaderT (\r -> ContT (f r))

runGadget ::
    Gadget cmd p s a
    -> Entity p s
    -> (a -> StateT J.JSString (Program cmd) ())
    -> StateT J.JSString (Program cmd) ()
runGadget x l = runContT (runReaderT x l)

gadgetWith :: WeakObj s -> Gadget cmd s s a -> ContT () (StateT J.JSString (Program cmd)) a
gadgetWith obj = (`runReaderT` (Entity obj id))

gadgetWith' :: Obj s -> Gadget cmd s s a -> ContT () (StateT J.JSString (Program cmd)) a
gadgetWith' obj = gadgetWith (weakObj obj)

evalGadget :: Gadget cmd p s () -> Entity p s -> StateT J.JSString (Program cmd) ()
evalGadget gad ent = evalContT . (`runReaderT` ent) $ gad
