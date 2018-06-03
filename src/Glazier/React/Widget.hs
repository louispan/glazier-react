{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Widget where

import Control.Lens
-- import Control.Lens.Misc
import Data.Bifunctor
-- import Data.Semigroup
-- import qualified GHC.Generics as G
import Control.Monad.Trans.AExcept
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.Window

type Widget cmd p s a = AExceptT (Window s ()) (Gadget cmd p s) a

-- type Widget' cmd p s = Gadget cmd p s (Window s ())

-- magnifyWidget' :: Lens' t s -> Gadget cmd p s (Window s ()) -> Gadget cmd p t (Window t ())
-- magnifyWidget' l gad = (enlargeScene l) <$> (enlargeEntity l gad)

-- magnifyWidget' :: Lens' t s -> Gadget cmd p s (Either (Window s ()) a) -> Gadget cmd p t (Either (Window t ()) a)
-- magnifyWidget' l gad = (first (enlargeScene l)) <$> (enlargeEntity l gad)

magnifyWidget :: Lens' t s -> AExceptT (Window s ()) (Gadget cmd p s) a -> AExceptT (Window t ()) (Gadget cmd p t) a
magnifyWidget l wid = aexceptT ((first (enlargeScene l)) <$> (enlargeEntity l (runAExceptT wid)))

-- data Widget cmd p s a = Widget
--     { window :: Window s () -- so it can read IORef
--     , gadget :: Gadget cmd p s a
--     } deriving (G.Generic, Functor)

-- makeLenses_ ''Widget

-- overGadget2 ::
--     (Gadget c1 p1 s a1 -> Gadget c2 p2 s a2 -> Gadget c3 p3 s a3)
--     -> Widget c1 p1 s a1 -> Widget c2 p2 s a2 -> Widget c3 p3 s a3
-- overGadget2 f (Widget dis1 ini1) (Widget dis2 ini2) =
--     Widget
--     (dis1 <> dis2)
--     (f ini1 ini2)

-- -- overGadget :: (Gadget c1 p1 s a1 -> Gadget c2 p2 s a2) -> Widget c1 p1 s a1 -> Widget c2 p2 s a2
-- -- overGadget f = _gadget %~ f

-- -- overWindow :: (Window s () -> Window s ()) -> Widget c p s a -> Widget c p s a
-- -- overWindow f = _window %~ f

-- ------------------------------------------

-- instance Applicative (Widget cmd p s) where
--     pure a = Widget mempty (pure a)
--     (<*>) = overGadget2 (<*>)

-- -- merge ContT together by pre-firing the left ContT's output.
-- -- That is, the resultant ContT will fire the output twice.
-- instance Semigroup (Widget cmd p s a) where
--     (<>) = overGadget2 (<>)

-- instance Monoid (Widget cmd p s a) where
--     mempty = Widget mempty mempty
--     mappend = overGadget2 mappend

-- prototype :: Widget cmd p s ()
-- prototype = mempty

-- enlargeModel :: Lens' s' s -> Widget cmd p s a -> Widget cmd p s' a
-- enlargeModel l (Widget win gad) = Widget (enlargeScene l win) (enlargeEntity l gad)

