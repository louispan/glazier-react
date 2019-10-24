{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Widget where

import Control.Also
import Control.Applicative
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Observer
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.String
import Data.Tagged.Extras
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Markup
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.ReactId
import Glazier.React.ReactPath
import qualified JavaScript.Extras as JE
import System.Mem.Weak

type AskScratch = MonadAsk' (Tagged "Scratch" JE.Object)
instance {-# OVERLAPPING #-} Monad m => MonadAsk (Tagged "Scratch" JE.Object) (Tagged "Scratch" JE.Object) (ReaderT (Tagged "Scratch" JE.Object) m) where
    askEnviron _ = ask
askScratch :: AskScratch m => m JE.Object
askScratch = (untag' @"Scratch") <$> askEnviron @(Tagged "Scratch" JE.Object) Proxy

scratchAccessor :: ReactId -> J.JSString -> J.JSString
scratchAccessor i n = "$" <> (fromString $ show $ unReactId i) <> "_" <> n

deleteScratch :: (MonadIO m, AskScratch m) => ReactId -> J.JSString -> m ()
deleteScratch i n = do
    d <- askScratch
    liftIO $ JE.deleteProperty d (scratchAccessor i n)

setScratch :: (MonadIO m, AskScratch m, JE.ToJS a) => ReactId -> J.JSString -> a -> m ()
setScratch i n v = do
    d <- askScratch
    liftIO $ JE.setProperty d (scratchAccessor i n) (JE.toJS v)

getScratch :: (MonadIO m, AskScratch m) => ReactId -> J.JSString -> m J.JSVal
getScratch i n = do
    d <- askScratch
    liftIO $ JE.getProperty d (scratchAccessor i n)

scratchXTimes :: (MonadIO m, AskScratch m) => Int -> ReactId -> J.JSString -> m () -> m ()
scratchXTimes maxTimes i n m = do
    d <- JE.fromJS @Int <$> getScratch i n
    let (x', m') = case (d, maxTimes) of
            (_, x) | x <= 0         -> (0, pure ())
            (Nothing, _)            -> (1, m)
            (Just y, x) | y < x     -> (y + 1, m)
            _                       -> (maxTimes, pure ())
    setScratch i n x'
    m'

type AskModelWeakVar s = MonadAsk "ModelWeakVar" (Tagged "ModelWeakVar" (Weak (MVar s)))
instance {-# OVERLAPPING #-} Monad m => MonadAsk "ModelWeakVar" (Tagged "ModelWeakVar" (Weak (MVar s))) (ReaderT (Tagged "ModelWeakVar" (Weak (MVar s))) m) where
    askEnviron _ = ask
askModelWeakVar :: AskModelWeakVar s m => m (Weak (MVar s))
askModelWeakVar = (untag' @"ModelWeakVar") <$> askEnviron @"ModelWeakVar" Proxy

type AskModel s = MonadAsk "Model" (Tagged "Model" s)
instance {-# OVERLAPPING #-} Monad m => MonadAsk "Model" (Tagged "Model" s) (ReaderT (Tagged "Model" s) m) where
    askEnviron _ = ask
askModel :: AskModel s m => m s
askModel = (untag' @"Model") <$> askEnviron @"Model" Proxy

-- | This is like 'view' but for the 'AskModel', not 'MonadReader'
model :: AskModel s m => Getting a s a -> m a
model l = (getConst #. l Const) <$> askModel

-- | This is like 'preview' but for the 'AskModel', not 'MonadReader'
premodel :: AskModel s m => Getting (First a) s a -> m (Maybe a)
premodel l = (getFirst #. foldMapOf l (First #. Just)) <$> askModel

type AskConstructor m = MonadObserver' (Tagged "Constructor" (Command m)) m
askConstructor :: forall m. AskConstructor m => m (Command m -> m ())
askConstructor = (. Tagged @"Constructor") <$> askObserver @(Tagged "Constructor" (Command m)) Proxy

type AskDestructor m = MonadObserver' (Tagged "Destructor" (Command m)) m
askDestructor :: forall m. AskDestructor m => m (Command m -> m ())
askDestructor = (. Tagged @"Destructor") <$> askObserver @(Tagged "Destructor" (Command m)) Proxy

type AskRendered m = MonadObserver' (Tagged "Rendered" (Command m)) m
askRendered :: forall m. AskRendered m => m (Command m -> m ())
askRendered = (. Tagged @"Rendered") <$> askObserver @(Tagged "Rendered" (Command m)) Proxy

-- | Register and execute the given monad at construction time.
-- At construction, 'initDestructor' and 'initRendered' may be used.
-- The given monad is only performed at construction of the widget.
-- That is, on subsequent rerendesrs, @initConstructor = const $ pure ()@
-- Do not expect this function to do anything on subsequent rerenders
-- so don't use the function conditionally or inside event handling code.
initConstructor :: forall m. (AskConstructor m, MonadCodify m) => m () -> m ()
initConstructor m = do
    f <- askConstructor
    c <- codify' m
    f c

-- | Register the given monad to be evaluated at destruction time.
-- The same "construction time only registration caveats" apply as in 'initConstructor'.
initDestructor :: forall m. (AskDestructor m, MonadCodify m) => m () -> m ()
initDestructor m = do
    f <- askDestructor
    c <- codify' m
    f c

-- | Register the given monad to be evaluated after every rerender, including the first rerender.
-- The same "construction time only registration caveats" apply as in 'initConstructor'.
initRendered :: forall m. (AskRendered m, MonadCodify m) => m () -> m ()
initRendered m = do
    f <- askRendered
    c <- codify' m
    f c

-- | 'Widget' is a concrete transformer stack of 'MonadWidget' and 'MonadGadget'
type Widget s c =
    ObserverT (Tagged "Rendered" c) -- 'AskRendered'
    (ObserverT (Tagged "Destructor" c) -- 'AskDestructor'
    (ObserverT (Tagged "Constructor" c) -- 'AskConstructor'
    (ReaderT (Tagged "ModelWeakVar" (Weak (MVar s))) -- 'AskModelWeakVar'
    (ReaderT (Tagged "Model" s) -- 'AskModel'
    (ReaderT (Tagged "Scratch" JE.Object) -- 'AskScratch'
    (ReaderT (Weak (IORef Notifier)) -- 'AskNotifierWeakRef'
    (ReaderT (Weak (IORef Plan)) -- 'AskPlanWeakRef', 'AskLogLevel', 'AskLogCallStackDepth', 'AskLogName'
    (MaybeT -- 'Alternative'
    (ContT () -- 'MonadDelegate'
    -- State monads must be inside ContT to be a 'MonadDelegate'
    (StateT ReactPath -- 'PutReactPath', 'AskReactPath'
    (StateT (DL.DList ReactMarkup) -- 'PutMarkup'
    (ProgramT c IO -- 'MonadComand', 'MonadIO'
    ))))))))))))

-- -- | Run a widget on an @Obj t@
-- -- The markup, initialization (initConstructor, etc) effects are ignored.
-- runWidget :: (MonadIO m, MonadCommand m, Cmd' [] (Command m)) => Widget t (Command m) a -> Obj t -> m a
-- runWidget wid (Obj plnRef plnWkRef _ notifierWkRef mdlVar mdlWkVar) = do
--     mdl <- liftIO $ readMVar mdlVar
--     o <- liftIO $ scratch <$> readIORef plnRef
--     -- get the commands from running the widget using the refs/var from the given
--     delegatify $ \f -> do
--         let wid' = wid >>= instruct . f
--         cs <- liftIO $ execProgramT'
--             $ (`evalStateT` mempty) -- markup
--             $ (`evalStateT` (ReactPath (Nothing, [])))
--             $ evalContT
--             $ (`evalMaybeT` ())
--             $ (`runReaderT` plnWkRef)
--             $ (`runReaderT` notifierWkRef)
--             $ (`runReaderT` Tagged @"Scratch" o)
--             $ (`runReaderT` Tagged @"Model" mdl)
--             $ (`runReaderT` Tagged @"ModelWeakVar" mdlWkVar)
--             $ (`runObserverT` (const $ pure ()))
--             $ (`runObserverT` (const $ pure ()))
--             $ (`runObserverT` (const $ pure ()))
--             $ wid'
--         exec' (DL.toList cs)

-- | ALlow additional user ReaderT and IdentityT stack on top of @Widget s@
-- Like 'Control.Monad.IO.Unlift.UnliftIO', this newtype wrapper prevents impredicative types.
newtype UniftWidget s m = UniftWidget { unliftWidget :: forall a. m a -> Widget s (Command m) a }

-- | Similar to 'Control.Monad.IO.Unlift.MonadUnliftIO', except we want to unlift a @Widget s m a@.
-- This limits transformers stack to 'ReaderT' and 'IdentityT' on top of @Widget s m@
-- Example
-- @
-- unliftMkObj :: (MonadUnliftWidget s m, MonadGadget s m)
--     => m () -> LogName -> s -> m (Obj s)
-- unliftMkObj m logname s = do
--     u <- askUnliftWidget
--     mkObj (unliftWidget u m) logname s
-- @
class MonadUnliftWidget s m | m -> s where
    askUnliftWidget :: m (UniftWidget s m)

instance MonadUnliftWidget s (Widget s c) where
    askUnliftWidget = pure (UniftWidget id)

instance (Functor m, MonadUnliftWidget s m) => MonadUnliftWidget s (ReaderT r m) where
    askUnliftWidget = ReaderT $ \r ->
        (\u -> UniftWidget (unliftWidget u . flip runReaderT r)) <$> askUnliftWidget

instance (Functor m, MonadUnliftWidget s m) => MonadUnliftWidget s (IdentityT m) where
    askUnliftWidget = IdentityT $
        (\u -> UniftWidget (unliftWidget u . runIdentityT)) <$> askUnliftWidget

-- FIXME: protect constructor

-- | A newtype wrapper to indicate that only 'Glazier.React.Reactor.Internal.MonadGadget'
-- effect are allowed.
-- 'GadgetT' is an instance of 'Glazier.React.Reactor.Internal.MonadGadget'
-- 'GadgetT' is *not* an instance of 'Glazier.React.Reactor.Internal.MonadWidget'
newtype GadgetT f a = GadgetT { runGadgetT :: f a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , Alternative
        , MonadDelegate
        , MonadCodify
        , MonadProgram
        )

deriving via (IdentityT f) instance (Also a f) => Also a (GadgetT f)

instance MonadTrans GadgetT where
    lift = GadgetT

-- | This instance allows using "plain string" in 'txt', and in props for 'lf', and 'bh'
-- when using @OverloadedString@ with @ExtendedDefaultRules@
instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (GadgetT m a) where
    fromString = pure . fromString

instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (GadgetT m (Maybe a)) where
    fromString = pure . Just . fromString

-- | 'Gadget' is a concrete transformer stack which is instance of 'MonadGadget'
type Gadget s c =
    ReaderT (Tagged "ModelWeakVar" (Weak (MVar s))) -- 'AskModelWeakVar'
    (ReaderT (Tagged "Model" s) -- 'AskModel'
    (ReaderT (Tagged "Scratch" JE.Object) -- 'AskScratch'
    (ReaderT (Weak (IORef Notifier)) -- 'AskNotifierWeakRef'
    (ReaderT (Weak (IORef Plan)) -- 'AskPlanWeakRef', 'AskLogLevel', 'AskLogCallStackDepth', 'AskLogName'
    (MaybeT -- 'Alternative'
    (ContT () -- 'MonadDelegate'
    -- State monads must be inside ContT to be a 'MonadDelegate'
    (StateT ReactPath -- 'PutReactPath', 'AskReactPath' -- used for logging
    (ProgramT c IO -- 'MonadComand', 'MonadIO'
    ))))))))

-- | Run a gadget on an @Obj t@
-- The markup, initialization (initConstructor, etc) effects are ignored.
runGadget :: (MonadIO m, MonadCommand m, Cmd' [] (Command m)) => Gadget s (Command m) a -> Obj s -> m a
runGadget gad (Obj plnRef plnWkRef _ notifierWkRef mdlVar mdlWkVar) = do
    mdl <- liftIO $ readMVar mdlVar
    o <- liftIO $ scratch <$> readIORef plnRef
    -- get the commands from running the gadget using the refs/var from the args
    delegatify $ \f -> do
        let gad' = gad >>= instruct . f
        cs <- liftIO $ execProgramT'
            $ (`evalStateT` (ReactPath (Nothing, [])))
            $ evalContT
            $ (`evalMaybeT` ())
            $ (`runReaderT` plnWkRef)
            $ (`runReaderT` notifierWkRef)
            $ (`runReaderT` Tagged @"Scratch" o)
            $ (`runReaderT` Tagged @"Model" mdl)
            $ (`runReaderT` Tagged @"ModelWeakVar" mdlWkVar)
            $ gad'
        exec' (DL.toList cs)

-- -- | ALlow additional user ReaderT and IdentityT stack on top of @Gadget s@
-- -- Like 'Control.Monad.IO.Unlift.UnliftIO', this newtype wrapper prevents impredicative types.
-- newtype UniftGadget s m = UniftGadget { unliftGadget :: forall a. m a -> Gadget s (Command m) a }

-- -- | Similar to 'Control.Monad.IO.Unlift.MonadUnliftIO', except we want to unlift a @Gadget s m a@.
-- -- This limits transformers stack to 'ReaderT' and 'IdentityT' on top of @Gadget s m@
-- class MonadUnliftGadget s m | m -> s where
--     askUnliftGadget :: m (UniftGadget s m)

-- instance MonadUnliftGadget s (Gadget s c) where
--     askUnliftGadget = pure (UniftGadget id)

-- instance (Functor m, MonadUnliftGadget s m) => MonadUnliftGadget s (ReaderT r m) where
--     askUnliftGadget = ReaderT $ \r ->
--         (\u -> UniftGadget (unliftGadget u . flip runReaderT r)) <$> askUnliftGadget

-- instance (Functor m, MonadUnliftGadget s m) => MonadUnliftGadget s (IdentityT m) where
--     askUnliftGadget = IdentityT $
--         (\u -> UniftGadget (unliftGadget u . runIdentityT)) <$> askUnliftGadget

