{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Core.Internal where

import Control.Also
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import Data.IORef
import Data.IORef.Extras
import qualified Data.JSString as J
import Data.Tagged
import Glazier.Command
import Glazier.Logger
import Glazier.React.Gadget.Internal
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.Reactant
import Glazier.React.ReactId
import Glazier.React.Reactor
import Glazier.React.ReactPath
import System.Mem.Weak

------------------------------------------------------
-- MonadGadget
------------------------------------------------------

-- | A 'MonadGadget'' is can log, 'instruct' 'Reactant' effects.
-- It can be safely turned into a 'Handler' and used in event handling code.
-- It is an instance of 'Alternative'. It is an instance of 'Also' so it can be combined.
class (CmdReactant (Command m)
        , AlternativeIO m, Also () m
        , MonadCont m
        , MonadLogger J.JSString m, AskLogName m, AskReactPath m
        , AskScratch m, AskPlanWeakRef m
        , AskNotifierWeakRef m
        -- , AskModel s m, AskModelWeakVar s m
        ) => MonadGadget' m where

    -- | Run a gadget action on an @Obj t@
    shall :: Obj s -> GadgetT (ModelT s m) a -> m a

infixl 2 `shall` -- lower than <|>

readModelWith :: Weak (MVar s) -> IO (Maybe s)
readModelWith mdlWkVar = do
    mdlVar' <- deRefWeak mdlWkVar
    case mdlVar' of
        Nothing -> pure Nothing
        Just mdlVar'' -> Just <$> readMVar mdlVar''

modelStateWith :: Weak (MVar s) -> ModifyModel s
modelStateWith mdlWkVar = ModifyModel $ \m -> do
    mdlVar' <- deRefWeak mdlWkVar
    case mdlVar' of
        Nothing -> pure Nothing
        Just mdlVar'' -> do
            s <- takeMVar mdlVar''
            case runState (runMaybeT m) s of
                (Nothing, _) -> do
                    putMVar mdlVar'' s
                    pure Nothing
                (Just a, s') -> do
                    putMVar mdlVar'' s'
                    pure (Just a)

instance (CmdReactant c, c ~ Command (Reactor c)) => MonadGadget' (Reactor c) where
    shall (Obj plnRef plnWkRef _ notifierWkRef mdlVar mdlWkVar) (GadgetT m) = do
        mdl <- liftIO $ readMVar mdlVar
        sch <- liftIO $ scratch <$> readIORef plnRef

        -- unwrap the ReaderT layers
        let m' = (`runReaderT` plnWkRef)
                . (`runReaderT` notifierWkRef)
                . (`runReaderT` (Tagged @"Scratch" sch))
                . (`runReaderT` (const $ pure ()))
                . (`runReaderT` (const $ pure ()))
                . (`runReaderT` (const $ pure ()))
                . runReactor
                . (`runModelT` (Just mdl, readModelWith mdlWkVar, modelStateWith mdlWkVar))
                $ m

        -- lift them into this monad
        Reactor
         . lift -- AskRendered
         . lift -- AskDestructor
         . lift -- AskConstructor
         . lift -- AskScratch
         . lift -- AskNotifierWeakRef
         . lift -- AskPlanWeakRef
         $ m'

instance (MonadGadget' m) => MonadGadget' (ModelT s m) where
    obj `shall` (GadgetT m) = do
        f <- askModelEnviron
        -- unwrap the ReaderT layers of this instance's ModelT
        -- m :: ModelT t (ModelT s m)
        -- m' :: ModelT t m
        let m' = hoist (`runModelT` f) m
            -- m'' :: m
            m'' = obj `shall` GadgetT m'
        lift m'' -- lift into ModelT

instance (MonadGadget' m) => MonadGadget' (IdentityT m) where
    obj `shall` (GadgetT m) = IdentityT $ obj `shall` GadgetT (hoist runIdentityT m)


instance (MonadGadget' m) => MonadGadget' (ReaderT r m) where
    obj `shall` (GadgetT m) = do
        r <- ask
        lift $ obj `shall` GadgetT (hoist (`runReaderT` r) m)


instance (MonadGadget' m) => MonadGadget' (GadgetT m) where
    obj `shall` (GadgetT m) = GadgetT $ obj `shall` GadgetT (hoist runGadgetT m)

type MonadGadget s m = (MonadGadget' m, MonadModel s m)

------------------------------------------------------
-- MonadWidget
------------------------------------------------------

-- A 'MonadWidget' is a 'MonadGadget' that additionally have access to
-- 'initConstructor', 'initDestructor', 'initRendered',
-- can generate 'Markup' and so should not be be for event handling, sice those
-- additional effects are ignored inside event handling.
-- 'GadgetT' is *not* an instance of 'MonadWidget'
class (CmdReactant (Command m)
    , MonadGadget' m, PutMarkup m, PutReactPath m
    , AskConstructor m, AskDestructor m, AskRendered m) => MonadWidget' m

instance {-# OVERLAPPABLE #-} (CmdReactant c) => MonadWidget' (Reactor c)

instance {-# OVERLAPPABLE #-} (MonadWidget' m) => MonadWidget' (ModelT s m)

instance {-# OVERLAPPABLE #-} (MonadWidget' m) => MonadWidget' (IdentityT m)

instance {-# OVERLAPPABLE #-} (MonadWidget' m) => MonadWidget' (ReaderT r m)

type MonadWidget s m = (MonadWidget' m, MonadModel s m)

------------------------------------------------------
-- Internal functions
------------------------------------------------------

mkReactId :: (MonadGadget' m) => m ReactId
mkReactId = delegatify $ exec' . MkReactId

mkModel :: (MonadGadget' m) => s -> m (IORef Notifier, Weak (IORef Notifier), MVar s, Weak (MVar s))
mkModel s = do
    i <- mkReactId
    notifierRef <- liftIO $ newIORef $ Notifier i mempty
    notifierWkRef <- liftIO $ mkWeakIORef notifierRef $ do
        ws <- liftIO $ watchers <$> readIORef notifierRef
        foldMap (unregisterFromNotifier i) ws
    mdlVar <- liftIO $ newMVar s
    mdlWkVar <- liftIO $ mkWeakMVar mdlVar (pure ())
    pure (notifierRef, notifierWkRef, mdlVar, mdlWkVar)
  where
    unregisterFromNotifier :: ReactId -> Weak (IORef Plan) -> IO ()
    unregisterFromNotifier i plnWkRef = (`evalMaybeT` ()) $ do
        plnRef <- guardJustIO $ deRefWeak plnWkRef
        liftIO $ atomicModifyIORef_' plnRef (_notifiers.at i .~ Nothing)

watchModel :: MonadIO m => (IORef Plan, Weak (IORef Plan)) -> (IORef Notifier, Weak (IORef Notifier)) -> m ()
watchModel (plnRef, plnWkRef) (notifierRef, notifierWkRef) = do
    notiId <- liftIO $ notifierId <$> readIORef notifierRef
    watcherId <- liftIO $ planId <$> readIORef plnRef
    liftIO $ atomicModifyIORef_' notifierRef (_watchers.at watcherId .~ Just plnWkRef)
    liftIO $ atomicModifyIORef_' plnRef (_notifiers.at notiId .~ Just notifierWkRef)

unwatchModel :: MonadIO m => IORef Plan -> IORef Notifier -> m ()
unwatchModel plnRef notifierRef = do
    notiId <- liftIO $ notifierId <$> readIORef notifierRef
    watcherId <- liftIO $ planId <$> readIORef plnRef
    liftIO $ atomicModifyIORef_' notifierRef (_watchers.at watcherId .~ Nothing)
    liftIO $ atomicModifyIORef_' plnRef (_notifiers.at notiId .~ Nothing)
