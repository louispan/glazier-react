{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Markup
    ( ReactMarkup(..)
    , BranchParam(..)
    , LeafParam(..)
    , mkFromMarkup
    , mkFromMarkups
    , ReactMlT(..)
    , ReactMl
    , fromElement
    , toElements
    , renderedWindow
    , txt
    , lf
    , bh
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
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Element as R
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- | The parameters required to create a branch ReactElement with children
data BranchParam = BranchParam
    { name :: J.JSString
    , properties :: R.Properties
    , children :: D.DList ReactMarkup
    }

-- | The parameters required to create a leaf ReactElement (no children)
data LeafParam = LeafParam
    { name :: J.JSVal -- ^ Can be a react component name
    , properties :: R.Properties
    }

data ReactMarkup
    = ElementMarkup R.ReactElement
    | TextMarkup J.JSString
    | BranchMarkup BranchParam
    | LeafMarkup LeafParam

-- | Create 'ReactElement's from a 'AtomMarkup'
mkFromMarkup :: ReactMarkup -> IO (R.ReactElement)
mkFromMarkup (BranchMarkup (BranchParam n p xs)) = do
    xs' <- sequenceA $ mkFromMarkup <$> (D.toList xs)
    R.mkBranchElement n p xs'

mkFromMarkup (LeafMarkup (LeafParam n p)) = R.mkLeafElement n p

mkFromMarkup (TextMarkup str) = pure $ R.textElement str

mkFromMarkup (ElementMarkup e) = pure e

-- | Create '[ReactElement]' from '[ReactMark]'
mkFromMarkups :: Traversable t => t ReactMarkup -> IO (t R.ReactElement)
mkFromMarkups xs = sequenceA $ mkFromMarkup <$> xs

-- | Monadic generator of ReactActom.
-- It is a CPS-style WriterT (ie a StateT) to build up a function
-- build up a computations to generate a '[AtomMarkup]'.
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

-- | To use an exisitng ReactElement
fromElement :: Applicative m => R.ReactElement -> ReactMlT m ()
fromElement e = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` ElementMarkup e)

-- | Convert the ReactMlt to [R.ReactElement]
toElements :: MonadIO io => ReactMlT io () -> io [R.ReactElement]
toElements m = do
    xs <- execStateT (runReactMlT m) mempty
    liftIO $ sequenceA $ mkFromMarkup <$> (D.toList xs)
-- | Handy function to render the ReactMlt under a Glazier window
renderedWindow :: MonadIO io => G.WindowT s (ReactMlT io) () -> G.WindowT s io [R.ReactElement]
renderedWindow = G.belowWindowT (toElements .)

-- | For text content
txt :: Applicative m => J.JSString -> ReactMlT m ()
txt n = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` TextMarkup n)

-- | For the contentless elements: eg 'br_'
lf :: Applicative m => J.JSVal -> [(T.Text, J.JSVal)] -> ReactMlT m ()
lf n props = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` LeafMarkup (LeafParam n $ HM.fromList props))

-- | For the contentful elements: eg 'div_'
bh :: Functor m => J.JSString -> [(T.Text, J.JSVal)] -> ReactMlT m a -> ReactMlT m a
bh n props (ReactMlT (StateT childs)) = ReactMlT . StateT $ \xs -> do
    (a, childs') <- childs mempty
    pure (a, xs `D.snoc` BranchMarkup (BranchParam n (HM.fromList props) childs'))
