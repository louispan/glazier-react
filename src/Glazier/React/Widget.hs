{-# LANGUAGE DataKinds #-}
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

data Widget cmd p s a = Widget
    { window :: Window s () -- so it can read IORef
    , gadget :: Gadget cmd p s a
    } deriving (G.Generic, Functor)

makeLenses_ ''Widget

overGadget2 ::
    (Gadget c1 p1 s a1 -> Gadget c2 p2 s a2 -> Gadget c3 p3 s a3)
    -> Widget c1 p1 s a1 -> Widget c2 p2 s a2 -> Widget c3 p3 s a3
overGadget2 f (Widget dis1 ini1) (Widget dis2 ini2) =
    Widget
    (dis1 <> dis2)
    (f ini1 ini2)

-- overGadget :: (Gadget c1 p1 s a1 -> Gadget c2 p2 s a2) -> Widget c1 p1 s a1 -> Widget c2 p2 s a2
-- overGadget f = _gadget %~ f

-- overWindow :: (Window s () -> Window s ()) -> Widget c p s a -> Widget c p s a
-- overWindow f = _window %~ f

------------------------------------------

instance Applicative (Widget cmd p s) where
    pure a = Widget mempty (pure a)
    (<*>) = overGadget2 (<*>)

-- merge ContT together by pre-firing the left ContT's output.
-- That is, the resultant ContT will fire the output twice.
instance Semigroup (Widget cmd p s a) where
    (<>) = overGadget2 (<>)

instance Monoid (Widget cmd p s a) where
    mempty = Widget mempty mempty
    mappend = overGadget2 mappend

blank :: Widget cmd p s ()
blank = mempty

enlargeModel :: Lens' s' s -> Widget cmd p s a -> Widget cmd p s' a
enlargeModel l (Widget win gad) = Widget (magnifyModel l win) (magnifySelf l gad)
