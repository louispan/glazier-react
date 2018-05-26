{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Widget where

import Control.Lens
import Control.Lens.Misc
import Data.Semigroup
import qualified GHC.Generics as G
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.Window

-- | Unforunately, because each widget contains callbacks
-- that has to be cleaned manually, we can't just rely on the garbage collector.
-- This contains 'Disposable' to be called on the *next* render.
-- This must be called before removing widgets from a containing model.
-- type Finalizer s m = s -> Ap m CD.Disposable

-- type Initializer x s m = (Widget x s m, MonadCont m)
-- type Handler x s m = (Widget x s m, MonadCont m)

-- (Arena p s)
-- (WithScene c p)

data Widget cmd p s a = Widget
    { window :: Window s () -- so it can read IORef
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
instance Semigroup (Widget cmd p s a) where
    (<>) = mapWidget2 (<>)

instance Monoid (Widget cmd p s a) where
    mempty = Widget mempty mempty
    mappend = mapWidget2 mappend

dummy :: Widget cmd p s ()
dummy = mempty

enlargeModel :: Traversal' s' s -> Widget cmd p s a -> Widget cmd p s' a
enlargeModel l (Widget win gad) = Widget (magnifyModel l win) (magnifySelf l gad)


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
