{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
import Glazier.Command.Exec
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.Model
import Glazier.React.Window

-- | A 'Widget' is a 'Gadget' that fires 'Either' a 'Window' or an event.
type Widget c o s = ExceptT (Window s ()) (Gadget c o s)

-- | Pass the same MonadWidget into this function to verify at compile time
-- that a concrete instance of widget doesn't require any @AsFacet (IO c) c@.
noIOWidget :: Widget (NoIOCmd c) s s a -> Widget c s s a -> Widget c s s a
noIOWidget _ = id

magnifyWidget :: Traversal' t s -> ExceptT (Window s ()) (Gadget c o s) a -> ExceptT (Window t ()) (Gadget c o t) a
magnifyWidget l wid = ExceptT $ (first (magnifiedModel l)) <$> (magnifiedEntity l (runExceptT wid))

-- | Convert a 'Gadget' into a 'Widget'
widget :: Gadget c o s (Either (Window s ()) a) -> Widget c o s a
widget = ExceptT

runWidget :: Widget c o s a -> Gadget c o s (Either (Window s ()) a)
runWidget = runExceptT

mapWidget ::
    (Gadget c o s (Either (Window s ()) a) -> Gadget c o' s' (Either (Window s' ()) b))
    -> Widget c o s a -> Widget c o' s' b
mapWidget = mapExceptT

display :: Window s () -> Widget c o s a
display = throwError

display' :: Window s () -> Widget c o s (Which '[])
display' = throwError

overWindow :: (Window s () -> Window s ()) -> Widget c o s a -> Widget c o s a
overWindow = withExceptT

overWindow2 :: (Window s () -> Window s () -> Window s ())
    -> Widget c o s a -> Widget c o s a -> Widget c o s a
overWindow2 f x y = withWindow x $ \x' -> withWindow y $ \y' -> display $ f x' y'

overWindow3 :: (Window s () -> Window s () -> Window s () -> Window s ())
    -> Widget c o s a -> Widget c o s a -> Widget c o s a -> Widget c o s a
overWindow3 f x y z = withWindow x $
    \x' -> withWindow y $
    \y' -> withWindow z $
    \z' -> display $ f x' y' z'

overWindow2' ::
    ( ChooseBoth x1 x2 ys)
    => (Window s () -> Window s () -> Window s ())
    -> Widget c o s (Which x1) -> Widget c o s (Which x2) -> Widget c o s (Which ys)
overWindow2' f x1 x2 = overWindow2 f (diversify <$> x1) (diversify <$> x2)

overWindow3' ::
    ( Diversify x1 ys
    , Diversify x2 ys
    , Diversify x3 ys
    , ys ~ AppendUnique x1 (AppendUnique x2 x3))
    => (Window s () -> Window s () -> Window s () -> Window s ())
    -> Widget c o s (Which x1) -> Widget c o s (Which x2) -> Widget c o s (Which x3) -> Widget c o s (Which ys)
overWindow3' f x1 x2 x3 = overWindow3 f
    (diversify <$> x1)
    (diversify <$> x2)
    (diversify <$> x3)

withWindow :: Widget c o s a -> (Window s () -> Widget c o s a) -> Widget c o s a
withWindow = catchError

withWindow' :: (ChooseBoth xs ys zs)
    => Widget c o s (Which xs)
    -> (Window s () -> Widget c o s (Which ys))
    -> Widget c o s (Which zs)
withWindow' m f = withWindow (diversify <$> m) (fmap diversify . f)
