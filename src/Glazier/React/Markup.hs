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
    , toReactElement
    , fromReactElement
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
import Data.Semigroup
import GHCJS.Types (JSString)
import qualified Glazier.React.Element as R

-- | The parameters required to create a ReactElement
data ReactMarkup =  ReactElem R.ReactElement | ReactText JSString | ReactAtom
    { name :: JSString
    , properties :: R.Properties
    , children :: [ReactMarkup]
    }

-- | Create 'ReactElement's from a 'ReactAtom'
fromReactMarkup :: ReactMarkup -> IO (R.ReactElement)
fromReactMarkup (ReactAtom n p xs) = do
    xs' <- sequenceA $ fromReactMarkup <$> xs
    R.createReactElement n p xs'

fromReactMarkup (ReactText str) = pure $ R.textReactElement str

fromReactMarkup (ReactElem e) = pure e

-- | Create a single 'ReactElement' from '[ReactMark]'
fromReactMarkups :: [ReactMarkup] -> IO (R.ReactElement)
fromReactMarkups xs = do
    xs' <- sequenceA $ fromReactMarkup <$> xs
    R.combineReactElements xs'

-- | Monadic generator of ReactActom.
-- It is a CPS-style WriterT (ie a StateT) to build up a function
-- build up a computations to generate a '[ReactAtom]'.
-- You can use 'runStateT' with an initial state of 'mempty'.
newtype ReactMlT m a = ReactMlT
    { runReactMlT :: StateT (D.DList ReactMarkup) m a
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

toReactElement :: MonadIO io => ReactMlT io a -> io (R.ReactElement)
toReactElement m = do
    xs <- execStateT (runReactMlT m) mempty
    liftIO $ fromReactMarkups (D.toList xs)

-- | To use an exisitng ReactElement
fromReactElement :: Applicative m => R.ReactElement -> ReactMlT m ()
fromReactElement e = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` ReactElem e)

-- | For text content
txt :: Applicative m => JSString -> ReactMlT m ()
txt n = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` ReactText n)

-- | For the contentless elements: eg 'br_'
leaf :: Applicative m => JSString -> R.Properties -> ReactMlT m ()
leaf n props = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` ReactAtom n props [])

-- | For the contentful elements: eg 'div_'
branch :: Functor m => JSString -> R.Properties -> ReactMlT m a -> ReactMlT m a
branch n props (ReactMlT (StateT childs)) = ReactMlT . StateT $ \xs -> do
    (a, childs') <- childs mempty
    pure (a, xs `D.snoc` ReactAtom n props (D.toList childs'))
