-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Glazier.Command
-- import Glazier.React.Entity
-- import Glazier.React.Obj
-- import Control.Also

-- | A 'Gadget' is an instance of 'MonadGadget'
-- The @s@ state can be magnified with 'magnifiedEntity'
type Gadget r c = ReaderT r (ContT () (Program c))

-- toGadget ::
--     (r
--         -> (a -> Program c ())
--         -> Program c ())
--     -> Gadget r c a
-- toGadget f = ReaderT (\r -> ContT (f r))

-- runGadget ::
--     Gadget r c a
--     -> r
--     -> (a -> Program c ())
--     -> Program c ()
-- runGadget x l = runContT (runReaderT x l)

-- gadgetWith :: GetWeakObj o s => o -> Gadget c s s a -> ContT () (Program c) a
-- gadgetWith obj = (`runReaderT` (Entity id (weakObj obj)))

-- evalGadget :: Gadget r c () -> r -> (Program c) ()
-- evalGadget gad r = evalContT . (`runReaderT` r) $ gad
