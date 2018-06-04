{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Widget where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.AExcept
import Data.Bifunctor
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.Window

-- | A 'Widget' is a 'Gadget' that fires 'Either' a 'Window' or a value.
type Widget cmd p s a = AExceptT (Window s ()) (Gadget cmd p s) a

magnifyWidget :: Lens' t s -> AExceptT (Window s ()) (Gadget cmd p s) a -> AExceptT (Window t ()) (Gadget cmd p t) a
magnifyWidget l wid = aexceptT $ (first (enlargeScene l)) <$> (enlargeEntity l (runAExceptT wid))

-- | Convert a 'Gadget' into a 'Widget'
widget :: Gadget cmd p s (Either (Window s ()) a) -> Widget cmd p s a
widget = aexceptT

runWidget :: Widget cmd p s a -> Gadget cmd p s (Either (Window s ()) a)
runWidget = runAExceptT

mapWidget :: (Gadget cmd p s (Either (Window s ()) a) -> Gadget cmd p' s' (Either (Window s' ()) b)) -> Widget cmd p s a -> Widget cmd p' s' b
mapWidget = mapAExceptT

overWindow :: (Window s () -> Window s ()) -> Widget cmd p s a -> Widget cmd p s a
overWindow = withAExceptT

display :: Window s () -> Widget cmd p s a
display = throwError
