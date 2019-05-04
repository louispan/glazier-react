-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Gadget where

import Glazier.Logger
import Glazier.React.Obj
import Glazier.React.ReactId

-- type MonadGadget' c s m= (Logger c m, WeakObjReader o m)

-- type MonadGadget c s m= (MonadGadget c s m, AskReactId m)

-- -- | Wrap ReaderTs around Gadget for instances of WeakObjReader and AskReactId
-- type Gadget c = ContT () (Program c)


-- import Control.Also
-- import Control.Monad.Trans.Cont
-- import Control.Monad.Trans.Reader
-- import Glazier.Command
-- import Glazier.Logger
-- import Glazier.React.Obj
-- -- import Glazier.React.Subject

-- -- | A 'Gadget' is an instance of 'MonadReactor'
-- -- The @s@ state can be magnified with 'magnifiedSubject'

-- instance GetWeakObj s r => MonadLogLevel (Gadget c r) where
--     -- logLevel :: m (Benign IO (Maybe LogLevel))
--     logLevel = do
--         obj <- view _getWeakObj
--         pure $ go obj
--       where
--         go obj = runMaybeT $ do
--             r <- sceneWeakRef obj
--             s <- MaybeT $ benignDeRefWeak r
--             pure . planLogLevel . plan $ s

-- toGadget ::
--     (r
--         -> (a -> Program c ())
--         -> Program c ())
--     -> Gadget c r a
-- toGadget f = ReaderT (\r -> ContT (f r))

-- runGadget ::
--     Gadget c r a
--     -> r
--     -> (a -> Program c ())
--     -> Program c ()
-- runGadget x l = runContT (runReaderT x l)

-- -- gadgetWith :: GetWeakObj o r => o -> Gadget c (WeakObj o) a -> ContT () (Program c) a
-- -- gadgetWith obj = (`runReaderT` (_getWeakObj obj))

-- -- gadgetWith :: GetWeakObj o r => o -> Gadget c (WeakObj o, ReactId) a -> ContT () (Program c) a
-- -- gadgetWith obj = (`runReaderT` (Subject id (_getWeakObj obj)))

-- evalGadget :: Gadget c r () -> r -> (Program c) ()
-- evalGadget gad r = evalContT . (`runReaderT` r) $ gad

