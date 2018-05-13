{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Glazier.React.Widget where

import Control.Concurrent
import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.ARWS.Strict
import Control.Monad.Trans.AState.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.IORef
import Data.Semigroup
import Data.Tagged
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.Markup
import Glazier.React.MkId.Internal
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Subject.Internal
import Glazier.React.Window
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

-- | Unforunately, because each widget contains callbacks
-- that has to be cleaned manually, we can't just rely on the garbage collector.
-- This contains 'Disposable' to be called on the *next* render.
-- This must be called before removing widgets from a containing model.
-- type Finalizer s m = s -> Ap m CD.Disposable

-- type Initializer x s m = (Widget x s m, MonadCont m)
-- type Handler x s m = (Widget x s m, MonadCont m)

-- (Arena p s)
-- (Scenario c p)

data Widget cmd p s a = Widget
    { window :: WindowT s IO () -- so it can read IORef
    , gadget :: Gadget cmd p s a
    } deriving (G.Generic, Functor)

makeLenses_ ''Widget

mapWidget2 ::
    (Gadget c1 p1 s a1 -> Gadget c2 p2 s a2 -> Gadget c3 p3 s a3)
    -> Widget c1 p1 s a1 -> Widget c2 p2 s a2 -> Widget c3 p3 s a3
mapWidget2 f (Widget dis1 ini1) (Widget dis2 ini2) =
    Widget
    (dis1 <> dis2)
    (f ini1 ini2)

------------------------------------------

instance Applicative (Widget cmd p s) where
    pure a = Widget mempty (pure a)
    (<*>) = mapWidget2 (<*>)

-- merge ContT together by pre-firing the left ContT's output.
-- That is, the resultant ContT will fire the output twice.
instance (Semigroup a) => Semigroup (Widget cmd p s a) where
    (<>) = mapWidget2 (<>)

instance (Monoid a) => Monoid (Widget cmd p s a) where
    mempty = Widget mempty mempty
    mappend = mapWidget2 mappend

dummy :: Widget cmd p s ()
dummy = mempty

enlargeModel :: Traversal' s' s -> Widget cmd p s a -> Widget cmd p s' a
enlargeModel l (Widget win gad) = Widget (magnifyModel l win) (magnifySelf l gad)


-- | Make an initialized 'Subject' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displaySubject' should be used to render the subject.
mkSubject ::
    ( MonadIO m
    , AsFacet [cmd] cmd
    )
    => (cmd -> m ())
    -> Widget cmd s s ()
    -> s
    -> m (Subject s)
mkSubject exec (Widget win gad) s = do
    scnRef <- liftIO $ newIORef (Scene newPlan s)
    scnVar <- liftIO $ newEmptyMVar
    let doRender = do
            -- render using from scnRef (doesn't block)
            scn <- readIORef scnRef
            (mrkup, _) <- execARWST win scn mempty -- ignore unit writer output
            JE.toJS <$> toElement mrkup
        doRef j = do
            -- update componentRef held in the Plan
            Scene pln mdl <- takeMVar scnVar
            putMVar scnVar $ Scene (pln & _componentRef .~ JE.fromJS j) mdl
        doRendered = join $ do
            -- Get the IO actions doOnRendered and reset the "Once" actions
            Scene pln mdl <- takeMVar scnVar
            let ((untag @"Once") -> x, (untag @"Always") -> y) = doOnRendered pln
            putMVar scnVar $ Scene (pln & _doOnRendered._1 .~ (Tagged @"Once" mempty)) mdl
            -- runs the "Once" actions before "Always".
            pure (x *> y)
        doListen ctx j = void $ runMaybeT $ do
            -- ctx is [ ElementalId, event name (eg OnClick) ]
            -- Javascript doesn't have tuples, only list
            (eid, n) <- MaybeT $ pure $ do
                ctx' <- JE.fromJS ctx
                case JA.toList ctx' of
                        [eid', n'] -> do
                            eid'' <- ElementalId <$> JE.fromJS eid'
                            n'' <- JE.fromJS n'
                            Just (eid'', n'')
                        -- malformed ctx, ignore
                        _ -> Nothing
            lift $ do
                mhdl <- do
                        Scene pln mdl <- takeMVar scnVar
                        -- get the handler for the elemental and event name.
                        -- also get the updated elemental as the "Once" handler may be reset.
                        -- returns (Maybe handler, Maybe newElemental)
                        -- First, look for elemental:
                        let (mhdl, gs') = at eid go (pln ^. _elementals)
                            go mg = case mg of
                                    Nothing -> (Nothing, Nothing)
                                    -- found elemental, check listeners for event name
                                    Just g -> let (ret, l) = at n go' (g ^. _listeners)
                                              in (ret, Just (g & _listeners .~ l))
                            go' ml = case ml of
                                    Nothing -> (Nothing, Nothing)
                                    -- Found listener with event name, reset "Once" actions
                                    Just ((untag @"Once") -> x, y) ->
                                        ( Just (x *> (untag @"Always" y))
                                        , Just (Tagged @"Once" mempty, y))
                        -- update the elemental with resetted "Once" listeners
                        -- and return the combined handler
                        putMVar scnVar $ Scene (pln & _elementals .~ gs') mdl
                        pure mhdl
                -- pass the javascript event arg into the combined handler
                case mhdl of
                    Nothing -> pure ()
                    Just hdl -> hdl (JE.JSRep j)

    renderCb <- liftIO $ J.syncCallback' doRender
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync doRef
    renderedCb <- liftIO $ J.syncCallback J.ContinueAsync doRendered
    listenCb <- liftIO $ J.syncCallback2 J.ContinueAsync doListen
    -- Create a MVar just for auto cleanup of the callbacks
    -- This mvar must not be reachable from the callbacks,
    -- otherwise the callbacks will always be alive.
    rel <- liftIO $ newEmptyMVar
    let cbs = ShimCallbacks renderCb renderedCb refCb listenCb
        pln = newPlan & _shimCallbacks .~ cbs
        scn = Scene pln s
        -- keep zombie alive as long as 'Subject' 'prolong' is reachable.
        sbj = Subject scnRef scnVar rel
        -- initalize the subject using the Gadget
        tick = runGadgetT gad (Entity sbj id) (const $ pure ())
        cs = execAState tick mempty
        cleanup = CD.runDisposable $ CD.dispose cbs
    liftIO $ do
        -- Create automatic garbage collection of the callbacks
        -- that will run when the zombieVar is garbage collected.
        void $ mkWeakMVar rel cleanup
        -- update the mutable variables
        atomicWriteIORef scnRef scn
        putMVar scnVar scn
    -- execute additional commands
    exec (command' $ DL.toList cs)
    -- return the initialized subject
    pure sbj
  where
    newPlan :: Plan
    newPlan = Plan
        Nothing
        (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
        (Tagged mempty, Tagged mempty)
        mempty

-- enlargePlan :: Traversal' (TVar Plan) (TVar Plan) -> Widget c p s a -> Widget c p s a
-- enlargePlan l (Widget disp ini) = Widget disp (magnifyObjPlan l ini)

-- -- | Wrap a gadget inside another 'ShimComponent' with its own 'Plan'
-- -- This results in a 'Widget' that can be composed with other 'Widgets'
-- toShim :: PlanId -> Gadget c t s a -> Widget c t s a
-- toShim pid gad = Widget (magnifyPlan (_plans.ix pid) shimWindow) (magnifyMyPlan (_plans.ix pid) gad)

-- magnifyMethod :: Monad m
--     => LensLike' f s a -> MethodT w x s m c1 -> MethodT w x s m c1
-- magnifyDisplay l disp = magnify (editScene l) disp

--    (magnify (to $ \(Traversal t) -> Traversal (t . l)) ini)

-- -- | Makes and initialzies a spec from a req.
-- -- Used by prototypes that contain other archetypes.
-- mkInitializedSpec :: Monad m
--     => (r -> m s)
--     -> MethodT s m c
--     -> MethodT r m (c, s)
-- mkInitializedSpec mkSpc ini = do
--     r <- ask
--     s <- lift $ lift $ mkSpc r
--     c <- magnify (to (const s)) ini
--     pure (c, s)


-- -- obviousHandler :: Handler s m a b -> Handler s m (Which '[a]) b
-- -- obviousHandler hdl = hdl . obvious

-- -- contramapHandler :: (a1 -> a2) -> Handler s m a2 b -> Handler s m a1 b
-- -- contramapHandler f hdl = hdl . f

-- -- mapHandler :: (b1 -> b2) -> Handler s m a b1 -> Handler s m a b2
-- -- mapHandler = fmap . fmap

-- -- memptyHandler :: Applicative m => Handler s m a b
-- -- memptyHandler = mempty

-- -- mappendHandler :: Applicative m => Handler s m a b -> Handler s m a b -> Handler s m a b
-- -- mappendHandler = mappend
-- -- infixr 6 `mappendHandler` -- like mappend

-- -- ignoreHandler :: forall a m s. Applicative m => Handler s m a ()
-- -- ignoreHandler = (const @_ @a) mempty

-- -- arrowHandler :: (a -> b) -> Handler s m a b
-- -- arrowHandler f = rk $ arr f

-- -- -- Chain the output from one handler into the input of the other.
-- --     Handler s m a b
-- --     -> Handler s m b c
-- --     -> Handler s m a c
-- -- -- intoH f g = f & E.rk2 (>>>) $ g
-- -- intoH f g = f >=> g

-- -- -- Chain the output from one handler into the input of the other
-- -- -- as much as possible. Any unhandled output is forwarded.
-- -- intoH' :: (Injected a2 b1 b2 b3)
-- --     => Handler s m a (Which b1)
-- --     -> Handler s m (Which a2) (Which b2)
-- --     -> Handler s m a (Which b3)
-- -- intoH' f g = f >=> E.underK1 injected g

-- -- -- Run th left handler and then the right handler with the same input,
-- -- -- and only fire events from the second input.
-- -- thenH :: Handler s m a () -> Handler s m a b -> Handler s m a b
-- -- thenH = ($!*>)

-- -- -- Run left and also the right handler with the same input, and combine the output type
-- -- -- A binary associative function for 'nulHandler'.
-- -- alsoH :: (Applicative m, ChooseBoth b1 b2 b3)
-- --     => Handler s m a (Which b1)
-- --     -> Handler s m a (Which b2)
-- --     -> Handler s m a (Which b3)
-- -- alsoH = liftA2 also
-- -- infixr 6 `alsoH` -- like mappend

-- -- maybeH :: Applicative m
-- --     => Handler s m a b
-- --     -> Handler s m (Maybe a) b
-- -- maybeH hdl = maybe mempty hdl
