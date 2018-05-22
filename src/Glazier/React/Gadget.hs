-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Control.Monad.Trans.ACont
import Control.Monad.Trans.AReader
import Control.Monad.Trans.AState.Strict
import qualified Data.DList as DL
import Glazier.React.Entity

type Gadget cmd p s = AReaderT (Entity p s) (AContT () (AState (DL.DList cmd)))

-- type MonadGadget cmd p s m =
--     ( MonadReader (Entity p s) m
--     , MonadDelegate ()  m
--     , MonadState (DL.DList cmd) m
--     )

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

evalGadget :: Gadget cmd p s () -> Entity p s -> AState (DL.DList cmd) ()
evalGadget gad ent = evalAContT . (`runAReaderT` ent) $ gad
