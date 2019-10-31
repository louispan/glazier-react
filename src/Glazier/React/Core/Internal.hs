{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Core.Internal where

import Control.Also
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Environ
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import Data.IORef
import Data.IORef.Extras
import qualified Data.JSString as J
import Data.Tagged.Extras
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.Reactant
import Glazier.React.ReactId
import Glazier.React.Reactor
import Glazier.React.ReactPath
import qualified JavaScript.Extras as JE
import System.Mem.Weak

------------------------------------------------------
-- MonadGadget
------------------------------------------------------

type MonadReactant m = (MonadIO m, MonadCommand m, CmdReactant (Command m))

-- | A 'MonadGadget'' is can log, 'instruct' 'Reactant' effects.
-- It can be safely turned into a 'Handler' and used in event handling code.
-- It is an instance of 'Alternative'. It is an instance of 'Also' so it can be combined.
class (CmdReactant (Command m)
        , AlternativeIO m, forall r. Also r m
        , MonadCont m
        , MonadLogger J.JSString m
        , MonadAsk' LogName m
        , MonadAsk' ReactPath m
        , AskScratch m
        , AskPlanWeakRef m
        , AskNotifierWeakRef m
        ) => MonadGadget' m where

    -- | Run a gadget action on an @Obj t@
    shall :: Obj s -> ModelT s m a -> m a

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
    shall (Obj plnRef plnWkRef _ notifierWkRef mdlVar mdlWkVar) m = do
        mdl <- liftIO $ readMVar mdlVar
        sch <- liftIO $ scratch <$> readIORef plnRef

        -- unwrap the ReaderT layers
        let m' = (`runReaderT` plnWkRef)
                . (`runReaderT` notifierWkRef)
                . (`runReaderT` (Tagged @"Scratch" sch))
                . runReactor
                . (`runModelT` (Just mdl, readModelWith mdlWkVar, modelStateWith mdlWkVar))
                $ m

        -- lift them into this monad
        Reactor
         . lift -- AskScratch
         . lift -- AskNotifierWeakRef
         . lift -- AskPlanWeakRef
         $ m'

instance (MonadGadget' m) => MonadGadget' (ModelT s m) where
    obj `shall` m = do
        f <- askModelEnv
        -- unwrap the ReaderT layers of this instance's ModelT
        -- m :: ModelT t (ModelT s m)
        -- m' :: ModelT t m
        let m' = hoist (`runModelT` f) m
            -- m'' :: m
            m'' = obj `shall` m'
        lift m'' -- lift into ModelT

instance (MonadGadget' m) => MonadGadget' (IdentityT m) where
    obj `shall` m = IdentityT $ obj `shall` (hoist runIdentityT m)


instance (MonadGadget' m) => MonadGadget' (ReaderT r m) where
    obj `shall` m = do
        r <- ask
        lift $ obj `shall` (hoist (`runReaderT` r) m)


type MonadGadget s m = (MonadGadget' m, MonadModel s m)

------------------------------------------------------
-- MonadWidget
------------------------------------------------------

-- | A 'MonadWidget'' is a 'MonadGadget'' that additionally can generate 'Markup'
-- and so should not be be for event handling.
class (CmdReactant (Command m)
    , MonadGadget' m
    , PutMarkup m
    , MonadPut' ReactPath m
    ) => MonadWidget' m

instance {-# OVERLAPPABLE #-} (CmdReactant c) => MonadWidget' (Reactor c)

instance {-# OVERLAPPABLE #-} (MonadWidget' m) => MonadWidget' (ModelT s m)

instance {-# OVERLAPPABLE #-} (MonadWidget' m) => MonadWidget' (IdentityT m)

instance {-# OVERLAPPABLE #-} (MonadWidget' m) => MonadWidget' (ReaderT r m)

-- | A 'MonadWidget' is  acces to the 'ModelModel' and ability to 'askUnliftWidget'
type MonadWidget s m = (MonadWidget' m, MonadModel s m, MonadUnliftWidget s m)

------------------------------------------------------
-- Internal functions
------------------------------------------------------

mkReactId :: (MonadCommand m, CmdReactant (Command m)) => m ReactId
mkReactId = delegatify $ exec' . MkReactId

mkModelRef :: (MonadReactant m) => s -> m (IORef Notifier, Weak (IORef Notifier), MVar s, Weak (MVar s))
mkModelRef s = do
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

watchModelRef :: MonadIO m => (IORef Plan, Weak (IORef Plan)) -> (IORef Notifier, Weak (IORef Notifier)) -> m ()
watchModelRef (plnRef, plnWkRef) (notifierRef, notifierWkRef) = do
    notiId <- liftIO $ notifierId <$> readIORef notifierRef
    watcherId <- liftIO $ planId <$> readIORef plnRef
    liftIO $ atomicModifyIORef_' notifierRef (_watchers.at watcherId .~ Just plnWkRef)
    liftIO $ atomicModifyIORef_' plnRef (_notifiers.at notiId .~ Just notifierWkRef)

unwatchModelRef :: MonadIO m => IORef Plan -> IORef Notifier -> m ()
unwatchModelRef plnRef notifierRef = do
    notiId <- liftIO $ notifierId <$> readIORef notifierRef
    watcherId <- liftIO $ planId <$> readIORef plnRef
    liftIO $ atomicModifyIORef_' notifierRef (_watchers.at watcherId .~ Nothing)
    liftIO $ atomicModifyIORef_' plnRef (_notifiers.at notiId .~ Nothing)

-- | This convert 'Handler' into a ghcjs 'Callback'
mkListener ::
    (MonadGadget' m)
    => Handler
    -> m Listener
mkListener f = do
    plnWkRef <- askPlanWeakRef
    delegatify $ exec' . MkListener plnWkRef f

sequenceProps :: MonadGadget' m
    => [(J.JSString, ModelT s m J.JSVal)]
    -> ModelT s m [(J.JSString, J.JSVal)]
sequenceProps props = concat <$> traverse f props
  where
    -- emit empty list if it fails, otherwise use the first one emitted
    f :: MonadGadget' m => (J.JSString, ModelT s m J.JSVal) -> ModelT s m [(J.JSString, J.JSVal)]
    f (n, m) = (`also` pure []) $ (\v -> [(n, v)]) <$> m

sequenceGadgets :: MonadGadget' m
    => [(J.JSString, m Handler)]
    -> m [(J.JSString, J.JSVal)]
sequenceGadgets gads = do
    gads' <- concat <$> traverse f gads -- :: m [(JString, Handler)]
    let gads'' = M.fromListWith (<>) gads' -- combine same keys together
        -- ElementComponent's ref callback is actuall elementRef, so rename ref to elementRef
        gads''' = case M.lookup "ref" gads'' of
                    Nothing -> gads''
                    Just v -> M.insertWith (<>) "elementRef" v gads''
        g = fmap JE.toJS . mkListener -- convert to JS callback
    traverse (traverse g) (M.toList gads''')
  where
    -- emit empty list if it fails, otherwise use the first one emitted
    f :: MonadGadget' m => (J.JSString, m Handler) -> m [(J.JSString, Handler)]
    f (n, m) = (`also` pure []) $ (\v -> [(n, v)]) <$> m
