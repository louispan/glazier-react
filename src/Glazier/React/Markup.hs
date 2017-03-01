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
    , BranchParam(..)
    , LeafParam(..)
    , fromMarkup
    , ReactMlT(..)
    , ReactMl
    , fromElement
    , toElements
    , markedWindow
    , markedElements
    , markedElement
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
    J.JSVal -- ^ Can be a react component type as well as html name
    R.Properties
    (D.DList ReactMarkup) -- ^ children

-- | The parameters required to create a leaf ReactElement (no children)
data LeafParam = LeafParam
    J.JSVal -- ^ Can be a react component type as well as html name
    R.Properties

data ReactMarkup
    = ElementMarkup R.ReactElement
    | TextMarkup J.JSString
    | BranchMarkup BranchParam
    | LeafMarkup LeafParam

-- | Create 'ReactElement's from a 'AtomMarkup'
fromMarkup :: ReactMarkup -> IO (R.ReactElement)
fromMarkup (BranchMarkup (BranchParam n p xs)) = do
    xs' <- sequenceA $ fromMarkup <$> (D.toList xs)
    R.mkBranchElement n p xs'

fromMarkup (LeafMarkup (LeafParam n p)) = R.mkLeafElement n p

fromMarkup (TextMarkup str) = pure $ R.textElement str

fromMarkup (ElementMarkup e) = pure e

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
    liftIO $ sequenceA $ fromMarkup <$> (D.toList xs)

-- | Render the ReactMlt under a Glazier window
markedWindow :: MonadIO io => G.WindowT s (ReactMlT io) () -> G.WindowT s io [R.ReactElement]
markedWindow = G.belowWindowT (toElements .)

-- | Fully render the ReactMlt into a [R.ReactElement]
markedElements :: MonadIO io => G.WindowT s (ReactMlT io) () -> s -> io [R.ReactElement]
markedElements w s = view G._WindowT' (markedWindow w) s

-- | Fully render the ReactMlt into a R.ReactElement
markedElement :: MonadIO io => G.WindowT s (ReactMlT io) () -> s -> io R.ReactElement
markedElement w s = markedElements w s >>= liftIO . R.mkCombinedElements

-- | For text content
txt :: Applicative m => J.JSString -> ReactMlT m ()
txt n = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` TextMarkup n)

-- | For the contentless elements: eg 'br_'
lf :: Applicative m => J.JSVal -> [(T.Text, J.JSVal)] -> ReactMlT m ()
lf n props = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` LeafMarkup (LeafParam n $ HM.fromList props))

-- | For the contentful elements: eg 'div_'
bh :: Functor m => J.JSVal -> [(T.Text, J.JSVal)] -> ReactMlT m a -> ReactMlT m a
bh n props (ReactMlT (StateT childs)) = ReactMlT . StateT $ \xs -> do
    (a, childs') <- childs mempty
    pure (a, xs `D.snoc` BranchMarkup (BranchParam n (HM.fromList props) childs'))
