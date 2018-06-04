{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Widget where

import Control.Lens
import Control.Monad.Except
import Data.Bifunctor
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.Window

-- | A 'Widget' is a 'Gadget' that fires 'Either' a 'Window' or a value.
type Widget cmd p s a = ExceptT (Window s ()) (Gadget cmd p s) a

magnifyWidget :: Lens' t s -> ExceptT (Window s ()) (Gadget cmd p s) a -> ExceptT (Window t ()) (Gadget cmd p t) a
magnifyWidget l wid = ExceptT $ (first (enlargeScene l)) <$> (enlargeEntity l (runExceptT wid))

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
