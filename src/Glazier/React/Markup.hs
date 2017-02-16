{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Markup
    ( ReactMarkup(..)
    , fromReactMarkup
    , fromReactMarkups
    , ReactMlT(..)
    , ReactMl
    , txt
    , leaf
    , branch
    ) where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.DList as D
import qualified Data.HashMap.Strict as M
import Data.Semigroup
import GHCJS.Types (JSString, JSVal)
import Glazier.React.Element

-- | The parameters required to create a ReactElement
data ReactMarkup = ReactText JSString | ReactAtom
    { name :: JSString
    , properties :: M.HashMap JSString JSVal
    , children :: [ReactMarkup]
    }

-- | Create 'ReactElement's from a 'ReactAtom'
fromReactMarkup :: ReactMarkup -> IO (ReactElement)
fromReactMarkup (ReactAtom n p xs) = do
    xs' <- sequenceA $ fromReactMarkup <$> xs
    createReactElement n p xs'

fromReactMarkup (ReactText str) = pure $ textReactElement str

-- | Create a single 'ReactElement' from '[ReactMark]'
fromReactMarkups :: [ReactMarkup] -> IO (ReactElement)
fromReactMarkups xs = do
    xs' <- sequenceA $ fromReactMarkup <$> xs
    combineReactElements xs'

-- | Monadic generator of ReactActom.
-- It is a CPS-style WriterT (ie a StateT) to build up a function
-- build up a computations to generate a '[ReactAtom]'.
-- You can use 'runStateT' with an initial state of 'mempty'.
newtype ReactMlT m a = ReactMlT
    { runReactMarkupT :: StateT (D.DList ReactMarkup) m a
    } deriving ( MonadState (D.DList ReactMarkup)
               , Monad
               , Applicative
               , Functor
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               , MFunctor
               )

type ReactMl = ReactMlT Identity

makeWrapped ''ReactMlT

instance (Semigroup a, Monad m) => Semigroup (ReactMlT m a) where
    (<>) = liftA2 (<>)

instance (Monoid a, Monad m) => Monoid (ReactMlT m a) where
    mempty = pure mempty
    mappend = liftA2 mappend

-- | For text content
txt :: Applicative m => JSString -> ReactMlT m ()
txt n = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` ReactText n)

-- | For the contentless elements: eg 'br_'
leaf :: Applicative m => JSString -> Properties -> ReactMlT m ()
leaf n props = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` ReactAtom n props [])

-- | For the contentful elements: eg 'div_'
branch :: Functor m => JSString -> Properties -> ReactMlT m a -> ReactMlT m a
branch n props (ReactMlT (StateT childs)) = ReactMlT . StateT $ \xs -> do
    (a, childs') <- childs mempty
    pure (a, xs `D.snoc` ReactAtom n props (D.toList childs'))
