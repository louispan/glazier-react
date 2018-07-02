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
import Glazier.React.Scene
import Glazier.React.Window

-- | A 'Widget' is a 'Gadget' that fires 'Either' a 'Window' or a value.
type Widget cmd p s a = ExceptT (Window s ()) (Gadget cmd p s) a

-- | Use this function to verify at compile time that the first widget doesn't
-- require any @AsFacet (IO cmd) cmd@.
noIOWidget :: Widget (NoIOCmd cmd) s s a -> Widget cmd s s a -> Widget cmd s s a
noIOWidget _ = id

magnifyWidget :: Traversal' t s -> ExceptT (Window s ()) (Gadget cmd p s) a -> ExceptT (Window t ()) (Gadget cmd p t) a
magnifyWidget l wid = ExceptT $ (first (magnifiedScene l)) <$> (magnifiedEntity l (runExceptT wid))

-- | Convert a 'Gadget' into a 'Widget'
widget :: Gadget cmd p s (Either (Window s ()) a) -> Widget cmd p s a
widget = ExceptT

runWidget :: Widget cmd p s a -> Gadget cmd p s (Either (Window s ()) a)
runWidget = runExceptT

mapWidget ::
    (Gadget cmd p s (Either (Window s ()) a) -> Gadget cmd p' s' (Either (Window s' ()) b))
    -> Widget cmd p s a -> Widget cmd p' s' b
mapWidget = mapExceptT

display :: Window s () -> Widget cmd p s a
display = throwError

overWindow :: (Window s () -> Window s ()) -> Widget cmd p s a -> Widget cmd p s a
overWindow = withExceptT

overWindow2 :: (Window s () -> Window s () -> Window s ())
    -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a
overWindow2 f x y = withWindow x $ \x' -> withWindow y $ \y' -> display $ f x' y'

overWindow3 :: (Window s () -> Window s () -> Window s () -> Window s ())
    -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a
overWindow3 f x y z = withWindow x $
    \x' -> withWindow y $
    \y' -> withWindow z $
    \z' -> display $ f x' y' z'

-- overWindow4 :: (Window s () -> Window s () -> Window s () -> Window s () -> Window s ())
--     -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a
-- overWindow4 f x y z a = withWindow x $
--     \x' -> withWindow y $
--     \y' -> withWindow z $
--     \z' -> withWindow a $
--     \a' -> display $ f x' y' z' a'

-- overWindow5 :: (Window s () -> Window s () -> Window s () -> Window s () -> Window s () -> Window s ())
--     -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a
-- overWindow5 f x y z a b = withWindow x $
--     \x' -> withWindow y $
--     \y' -> withWindow z $
--     \z' -> withWindow a $
--     \a' -> withWindow b $
--     \b' -> display $ f x' y' z' a' b'

overWindow2' ::
    ( ChooseBoth x1 x2 ys)
    => (Window s () -> Window s () -> Window s ())
    -> Widget cmd p s (Which x1) -> Widget cmd p s (Which x2) -> Widget cmd p s (Which ys)
overWindow2' f x1 x2 = overWindow2 f (diversify <$> x1) (diversify <$> x2)

overWindow3' ::
    ( Diversify x1 ys
    , Diversify x2 ys
    , Diversify x3 ys
    , ys ~ AppendUnique x1 (AppendUnique x2 x3))
    => (Window s () -> Window s () -> Window s () -> Window s ())
    -> Widget cmd p s (Which x1) -> Widget cmd p s (Which x2) -> Widget cmd p s (Which x3) -> Widget cmd p s (Which ys)
overWindow3' f x1 x2 x3 = overWindow3 f
    (diversify <$> x1)
    (diversify <$> x2)
    (diversify <$> x3)

-- overWindow4' ::
--     ( Diversify x1 ys
--     , Diversify x2 ys
--     , Diversify x3 ys
--     , Diversify x4 ys
--     , ys ~ AppendUnique x1 (AppendUnique x2 (AppendUnique x3 x4)))
--     => (Window s () -> Window s () -> Window s () -> Window s () -> Window s ())
--     -> Widget cmd p s (Which x1) -> Widget cmd p s (Which x2) -> Widget cmd p s (Which x3) -> Widget cmd p s (Which x4)
--     -> Widget cmd p s (Which ys)
-- overWindow4' f x1 x2 x3 x4 = overWindow4 f
--     (diversify <$> x1)
--     (diversify <$> x2)
--     (diversify <$> x3)
--     (diversify <$> x4)

-- overWindow5' ::
--     ( Diversify x1 ys
--     , Diversify x2 ys
--     , Diversify x3 ys
--     , Diversify x4 ys
--     , Diversify x5 ys
--     , ys ~ AppendUnique x1 (AppendUnique x2 (AppendUnique x3 (AppendUnique x4 x5))))
--     => (Window s () -> Window s () -> Window s () -> Window s () -> Window s () -> Window s ())
--     -> Widget cmd p s (Which x1) -> Widget cmd p s (Which x2) -> Widget cmd p s (Which x3)
--     -> Widget cmd p s (Which x4) -> Widget cmd p s (Which x5) -> Widget cmd p s (Which ys)
-- overWindow5' f x1 x2 x3 x4 x5 = overWindow5 f
--     (diversify <$> x1)
--     (diversify <$> x2)
--     (diversify <$> x3)
--     (diversify <$> x4)
--     (diversify <$> x5)

withWindow :: Widget cmd p s a -> (Window s () -> Widget cmd p s a) -> Widget cmd p s a
withWindow = catchError

withWindow' :: (ChooseBoth xs ys zs)
    => Widget cmd p s (Which xs)
    -> (Window s () -> Widget cmd p s (Which ys))
    -> Widget cmd p s (Which zs)
withWindow' m f = withWindow (diversify <$> m) (fmap diversify . f)

-- withWindow2 :: Widget cmd p s a -> Widget cmd p s a
--     -> (Window s () -> Window s () -> Widget cmd p s a)
--     -> Widget cmd p s a
-- withWindow2 x y f = withWindow x $ \x' -> withWindow y $ \y' -> f x' y'

-- withWindow3 :: Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a
--     -> (Window s () -> Window s () -> Window s () -> Widget cmd p s a)
--     -> Widget cmd p s a
-- withWindow3 x y z f = withWindow x $ \x' -> withWindow y $ \y' -> withWindow z $ \z' -> f x' y' z'

-- windowWith :: (Window s () -> Widget cmd p s a) -> Widget cmd p s a -> Widget cmd p s a
-- windowWith = flip withWindow

-- windowWith2 :: (Window s () -> Window s () -> Widget cmd p s a)
--     -> Widget cmd p s a -> Widget cmd p s a
--     -> Widget cmd p s a
-- windowWith2 f x y = withWindow2 x y f

-- windowWith3 :: (Window s () -> Window s () -> Window s () -> Widget cmd p s a)
--     -> Widget cmd p s a -> Widget cmd p s a -> Widget cmd p s a
--     -> Widget cmd p s a
-- windowWith3 f x y z = withWindow3 x y z f

-- withWindow2' ::
--     ( Diversify x1 zs
--     , Diversify x2 zs
--     , Diversify ys zs
--     , zs ~ AppendUnique x1 (AppendUnique x2 ys)) -- not redunant contraint
--     => Widget cmd p s (Which x1)
--     -> Widget cmd p s (Which x2)
--     -> (Window s () -> Window s () -> Widget cmd p s (Which ys))
--     -> Widget cmd p s (Which zs)
-- withWindow2' x y f = withWindow2 (diversify <$> x) (diversify <$> y) (\x' y' -> fmap diversify $ f x' y')

-- withWindow3' ::
--     ( Diversify x1 zs
--     , Diversify x2 zs
--     , Diversify x3 zs
--     , Diversify ys zs
--     , zs ~ AppendUnique x1 (AppendUnique x2 (AppendUnique x3 ys))) -- not redunant contraint
--     => Widget cmd p s (Which x1)
--     -> Widget cmd p s (Which x2)
--     -> Widget cmd p s (Which x3)
--     -> (Window s () -> Window s () -> Window s () -> Widget cmd p s (Which ys))
--     -> Widget cmd p s (Which zs)
-- withWindow3' x y z f = withWindow3 (diversify <$> x) (diversify <$> y) (diversify <$> z)
--     (\x' y' z' -> fmap diversify $ f x' y' z')

-- windowWith' :: (ChooseBoth xs ys zs)
--     => (Window s () -> Widget cmd p s (Which ys))
--     -> Widget cmd p s (Which xs)
--     -> Widget cmd p s (Which zs)
-- windowWith' f m = withWindow' m f

-- windowWith2' ::
--     ( Diversify x1 zs
--     , Diversify x2 zs
--     , Diversify ys zs
--     , zs ~ AppendUnique x1 (AppendUnique x2 ys)) -- not redunant contraint
--     => (Window s () -> Window s () -> Widget cmd p s (Which ys))
--     -> Widget cmd p s (Which x1)
--     -> Widget cmd p s (Which x2)
--     -> Widget cmd p s (Which zs)
-- windowWith2' f x y = withWindow2' x y f

-- windowWith3' ::
--     ( Diversify x1 zs
--     , Diversify x2 zs
--     , Diversify x3 zs
--     , Diversify ys zs
--     , zs ~ AppendUnique x1 (AppendUnique x2 (AppendUnique x3 ys))) -- not redunant contraint
--     => (Window s () -> Window s () -> Window s () -> Widget cmd p s (Which ys))
--     -> Widget cmd p s (Which x1)
--     -> Widget cmd p s (Which x2)
--     -> Widget cmd p s (Which x3)
--     -> Widget cmd p s (Which zs)
-- windowWith3' f x y z = withWindow3' x y z f
