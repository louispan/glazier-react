{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Widget where

import Control.Concurrent.MVar
import Control.Monad.Observer
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import Data.Tagged
import Glazier.Command
import Glazier.React.Markup
import Glazier.React.Obj.Internal
import Glazier.React.Plan
import Glazier.React.ReactPath
import System.Mem.Weak

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

-- | 'Gizmo' is an instance of 'MonadWidget'
-- Gizmo contains the effects (eg register listener)
-- as well as the html for rendering.
-- It is expected that the interpreter of the Gizmo will add a final effect
-- which is to set the final html for the component.
type Widget s c =
    ObserverT (Tagged "Rendered" c) -- 'AskRendered'
    (ObserverT (Tagged "Destructor" c) -- 'AskDestructor'
    (ObserverT (Tagged "Constructor" c) -- 'AskConstructor'
    (ReaderT (Weak (IORef Plan)) -- 'AskPlanWeakRef', 'AskLogLevel', 'AskLogCallStackDepth', 'AskLogName'
    (ReaderT (Weak (IORef Notifier)) -- 'AskNotifierWeakRef'
    (ReaderT (Tagged "ModelWeakVar" (Weak (MVar s))) -- 'AskModelWeakVar'
    (ReaderT (Tagged "Model" s) -- 'AskModel'
    (MaybeT -- 'Alternative'
    (ContT () -- 'MonadDelegate'
    -- State monads must be inside ContT to be a 'MonadDelegate'
    (StateT ReactPath -- 'PutReactPath', 'AskReactPath'
    (StateT (DL.DList ReactMarkup) -- 'PutMarkup'
    (ProgramT c IO -- 'MonadComand', 'MonadIO'
    )))))))))))

-- | Run a widget on an @Obj t@
-- The markup, initialization (initConstructor, etc) effects are ignored.
runWidget :: (MonadIO m, MonadCommand m, Cmd' [] (Command m)) => Widget t (Command m) a -> Obj t -> m a
runWidget wid (Obj _ plnWkRef _ notifierWkRef mdlVar mdlWkVar) = do
    mdl <- liftIO $ readMVar mdlVar
    -- get the commands from running the widget using the refs/var from the given
    delegatify $ \fire -> do
        let wid' = wid >>= instruct . fire
        cs <- liftIO $ execProgramT'
            $ (`evalStateT` mempty) -- markup
            $ (`evalStateT` (ReactPath (Nothing, [])))
            $ evalContT
            $ (`evalMaybeT` ())
            $ (`runReaderT` Tagged @"Model" mdl)
            $ (`runReaderT` Tagged @"ModelWeakVar" mdlWkVar)
            $ (`runReaderT` notifierWkRef)
            $ (`runReaderT` plnWkRef)
            $ (`runObserverT` (const $ pure ()))
            $ (`runObserverT` (const $ pure ()))
            $ (`runObserverT` (const $ pure ()))
            $ wid'
        exec' (DL.toList cs)

-- | ALlow additional user ReaderT and IdentityT stack on top of Widget c s
-- Like 'Control.Monad.IO.Unlift.UnliftIO', this newtype wrapper prevents impredicative types.
newtype UniftWidget s m = UniftWidget { unliftWidget :: forall a. m a -> Widget s (Command m) a }

-- | Similar to 'Control.Monad.IO.Unlift.MonadUnliftIO', except we want to unlift a @Widget (Gizmo c s) a@.
-- This limits transformers stack to 'ReaderT' and 'IdentityT' on top of @Gizmo c s m@
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



-- -- | Create a MonadState that run the given given a combining function
-- -- where the first arg is the state from running the markup producing MonadState with mempty,
-- -- and the 2nd arg the starting state of the resultant MonadState.
-- withWindow :: PutMarkup s m
--     => (Window s () -> Window s () -> Window s ())
--     -> m a
--     -> m a
-- withWindow f childs = do
--     -- save state
--     s <- askWindow
--     -- run children with mempty
--     putWindow mempty
--     a <- childs
--     childs' <- askWindow
--     -- restore state
--     putWindow s
--     modifyWindow (f childs')
--     pure a

-- import Control.Lens
-- import Control.Monad.Except
-- import Data.Bifunctor
-- import Data.Diverse.Lens
-- import Data.Diverse.Profunctor
-- import Glazier.Command.Exec
-- import Glazier.React.Entity
-- import Glazier.React.Gadget
-- import Glazier.React.Meta
-- import Glazier.React.Window

-- -- | A 'Widget' is a gadget that fires 'Either' a 'Window' or an event.
-- type MonadWidget c s m= (MonadGadget c s m, MonadError (Window s ()) m)

-- type Widget c s = ExceptT (Window s ()) (Gadget c)

-- -- -- | Pass the same MonadWidget into this function to verify at compile time
-- -- -- that a concrete instance of widget doesn't require any @AsFacet (IO c) c@.
-- -- -- LOUISFIXME: is there a simpler way @c ~ NoIOCmd c@?
-- -- noIOWidget :: Widget (NoIOCmd c) s s a -> Widget c s s a -> Widget c s s a
-- -- noIOWidget _ = id

-- -- magnifyWidget :: Traversal' t s -> ExceptT (Window s ()) (Gadget c s s') a -> ExceptT (Window t ()) (Gadget c o t) a
-- -- magnifyWidget l wid = ExceptT $ (first (magnifiedMeta l)) <$> (magnifiedEntity l (runExceptT wid))

-- -- | Convert a 'Gadget' into a 'Widget'
-- widget :: Gadget c s s' (Either (Window s ()) a) -> Widget c s s' a
-- widget = ExceptT

-- runWidget :: Widget c s s' a -> Gadget c s s' (Either (Window s ()) a)
-- runWidget = runExceptT

-- -- mapWidget ::
-- --     (Gadget c s s' (Either (Window s ()) a) -> Gadget c o' s' (Either (Window s' ()) b))
-- --     -> Widget c s s' a -> Widget c o' s' b
-- -- mapWidget = mapExceptT

-- display :: Window s () -> Widget c s s' a
-- display = throwError

-- display' :: Window s () -> Widget c s s' (Which '[])
-- display' = throwError

-- overWindow :: (Window s () -> Window s ()) -> Widget c s s' a -> Widget c s s' a
-- overWindow = withExceptT

-- overWindow2 :: (Window s () -> Window s () -> Window s ())
--     -> Widget c s s' a -> Widget c s s' a -> Widget c s s' a
-- overWindow2 f x y = withWindow x $ \x' -> withWindow y $ \y' -> display $ f x' y'

-- overWindow3 :: (Window s () -> Window s () -> Window s () -> Window s ())
--     -> Widget c s s' a -> Widget c s s' a -> Widget c s s' a -> Widget c s s' a
-- overWindow3 f x y z = withWindow x $
--     \x' -> withWindow y $
--     \y' -> withWindow z $
--     \z' -> display $ f x' y' z'

-- overWindow2' ::
--     ( ChooseBoth x1 x2 ys)
--     => (Window s () -> Window s () -> Window s ())
--     -> Widget c s s' (Which x1) -> Widget c s s' (Which x2) -> Widget c s s' (Which ys)
-- overWindow2' f x1 x2 = overWindow2 f (diversify <$> x1) (diversify <$> x2)

-- overWindow3' ::
--     ( Diversify x1 ys
--     , Diversify x2 ys
--     , Diversify x3 ys
--     , ys ~ AppendUnique x1 (AppendUnique x2 x3))
--     => (Window s () -> Window s () -> Window s () -> Window s ())
--     -> Widget c s s' (Which x1) -> Widget c s s' (Which x2) -> Widget c s s' (Which x3) -> Widget c s s' (Which ys)
-- overWindow3' f x1 x2 x3 = overWindow3 f
--     (diversify <$> x1)
--     (diversify <$> x2)
--     (diversify <$> x3)

-- withWindow :: Widget c s s' a -> (Window s () -> Widget c s s' a) -> Widget c s s' a
-- withWindow = catchError

-- withWindow' :: (ChooseBoth xs ys zs)
--     => Widget c s s' (Which xs)
--     -> (Window s () -> Widget c s s' (Which ys))
--     -> Widget c s s' (Which zs)
-- withWindow' m f = withWindow (diversify <$> m) (fmap diversify . f)
