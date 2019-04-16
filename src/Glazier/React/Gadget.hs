-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Control.Also
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Glazier.Command
import Glazier.Logger
import Glazier.React.Obj
import Glazier.React.Subject

-- type MonadGadget s r c m = (MonadReactor c m, HasWeakObj s r, Has ReactId r)

-- | A 'Gadget' is an instance of 'MonadReactor'
-- The @s@ state can be magnified with 'magnifiedSubject'
type Gadget c o s = ReaderT (Subject o s) (ContT () (Program c))

instance MonadLogLevel (Gadget c o s) where
    -- logLevel :: m (Benign IO (Maybe LogLevel))
    logLevel = do
        obj <- view (to self)
        pure $ go obj
      where
        go obj = runMaybeT $ do
            r <- sceneWeakRef obj
            s <- MaybeT $ benignDeRefWeak r
            pure . planLogLevel . plan $ s

-- toGadget ::
--     (Entity o s
--         -> (a -> Program c ())
--         -> Program c ())
--     -> Gadget c o s a
-- toGadget f = ReaderT (\r -> ContT (f r))

-- runGadget ::
--     Gadget c o s a
--     -> Entity o s
--     -> (a -> Program c ())
--     -> Program c ()
-- runGadget x l = runContT (runReaderT x l)

-- wack :: Gadget c o s a -> Gadget c o s a -> Gadget c o s a
-- wack = also


-- gadgetWith :: GetWeakObj o s => o -> Gadget c s s a -> ContT () (Program c) a
-- gadgetWith obj = (`runReaderT` (Entity id (weakObj obj)))

-- evalGadget :: Gadget c o s () -> Entity o s -> (Program c) ()
-- evalGadget gad ent = evalContT . (`runReaderT` ent) $ gad
