{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget where

import Control.Lens
import Control.Monad.Except
import Data.Bifunctor
import Data.Diverse.Lens
import Data.Diverse.Profunctor
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.Scene
import Glazier.React.Window

-- | A 'Widget' is a 'Gadget' that fires 'Either' a 'Window' or a value.
type Widget cmd p s a = ExceptT (Window s ()) (Gadget cmd p s) a

magnifyWidget :: Traversal' t s -> ExceptT (Window s ()) (Gadget cmd p s) a -> ExceptT (Window t ()) (Gadget cmd p t) a
magnifyWidget l wid = ExceptT $ (first (magnifiedScene l)) <$> (magnifiedEntity l (runExceptT wid))

-- | Convert a 'Gadget' into a 'Widget'
widget :: Gadget cmd p s (Either (Window s ()) a) -> Widget cmd p s a
widget = ExceptT

runWidget :: Widget cmd p s a -> Gadget cmd p s (Either (Window s ()) a)
runWidget = runExceptT

mapWidget :: (Gadget cmd p s (Either (Window s ()) a) -> Gadget cmd p' s' (Either (Window s' ()) b)) -> Widget cmd p s a -> Widget cmd p' s' b
mapWidget = mapExceptT

overWindow :: (Window s () -> Window s ()) -> Widget cmd p s a -> Widget cmd p s a
overWindow = withExceptT

display :: Window s () -> Widget cmd p s a
display = throwError

withWindow :: Widget cmd p s a -> (Window s () -> Widget cmd p s a) -> Widget cmd p s a
withWindow = catchError

withWindow' :: (ChooseBoth xs ys zs) => Widget cmd p s (Which xs) -> (Window s () -> Widget cmd p s (Which ys)) -> Widget cmd p s (Which zs)
withWindow' m f = catchError (diversify <$> m) (fmap diversify . f)
