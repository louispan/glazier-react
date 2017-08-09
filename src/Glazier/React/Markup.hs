{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Markup
    ( Listener
    , ReactMarkup(..)
    , BranchParam(..)
    , LeafParam(..)
    , fromMarkup
    , ReactMlT(..)
    , ReactMl
    , fromElement
    , toElements
    , txt
    , lf
    , bh
    -- * Glazier Window
    , markedWindow
    , markedElements
    , markedElement
    ) where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.DList as D
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Element as R
import qualified JavaScript.Extras as JE

type Listener = (J.JSString, J.Callback (J.JSVal -> IO ()))

-- | The parameters required to create a branch ReactElement with children
data BranchParam = BranchParam
    JE.JSVar
    [Listener]
    [JE.Property]
    [ReactMarkup]

-- | The parameters required to create a leaf ReactElement (no children)
data LeafParam = LeafParam
    JE.JSVar
    [Listener]
    [JE.Property]

data ReactMarkup
    = ElementMarkup R.ReactElement
    | TextMarkup J.JSString
    | BranchMarkup BranchParam
    | LeafMarkup LeafParam

-- | Create 'ReactElement's from a 'ReactMarkup'
fromMarkup :: ReactMarkup -> IO (R.ReactElement)
fromMarkup (BranchMarkup (BranchParam n ls props xs)) = do
    xs' <- sequenceA $ fromMarkup <$> xs
    R.mkBranchElement n (dedupListeners ls <> props) xs'

fromMarkup (LeafMarkup (LeafParam n ls props)) = R.mkLeafElement n (dedupListeners ls <> props)

fromMarkup (TextMarkup str) = pure $ R.textElement str

fromMarkup (ElementMarkup e) = pure e

-- | Monadic generator of ReactMarkup.
-- It is a CPS-style WriterT (ie a StateT) to build up a function
-- build up a computations to generate a '[ReactMarkup]'.
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

instance MonadTrans ReactMlT where
    lift = ReactMlT . lift

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
    liftIO $ sequenceA $ fromMarkup <$> D.toList xs

-- | For text content
txt :: Applicative m => J.JSString -> ReactMlT m ()
txt n = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` TextMarkup n)

-- | For the contentless elements: eg 'br_'
-- It is ok to have duplicate keys in cbs, however, it is a hidden error
-- to have duplicate keys in hdls, or across props and hdls.
-- It is possible to put callback in the props argument
-- but it is safer to put them in the cbs arguments because
-- multiple callbacks of the same key will be combined.
lf
    :: Monad m
    => JE.JSVar
    -> [Listener]
    -> [JE.Property]
    -> ReactMlT m ()
lf n ls props = ReactMlT . StateT $ \xs -> pure ((), xs `D.snoc` LeafMarkup (LeafParam n ls props))

-- | For the contentful elements: eg 'div_'
-- It is ok to have duplicate keys in cbs, however, it is a hidden error
-- to have duplicate keys in props, or across props and cbs.
-- It is possible to put callback in the props argument
-- but it is safer to put them in the cbs arguments because
-- multiple callbacks of the same key will be combined.
bh
    :: Monad m
    => JE.JSVar
    -> [Listener]
    -> [JE.Property]
    -> ReactMlT m a
    -> ReactMlT m a
bh n ls props (ReactMlT (StateT childs)) = ReactMlT . StateT $ \xs -> do
    (a, childs') <- childs mempty
    pure (a, xs `D.snoc` BranchMarkup (BranchParam n ls props (D.toList childs')))

-- | dedups a list of (key, Callback1) by merging callbacks for the same key together.
dedupListeners :: [Listener] -> [JE.Property]
dedupListeners = M.toList . M.fromListWith js_combineCallback1 . fmap (fmap JE.toJS')

-------------------------------------------------

-- | Render the ReactMlt under a Glazier window
markedWindow :: MonadIO io => G.WindowT s (ReactMlT io) () -> G.WindowT s io [R.ReactElement]
markedWindow = G._WRMT %~ (toElements' .)
  where
    toElements' :: MonadIO io => ReactMlT io (Maybe ()) -> io (Maybe [R.ReactElement])
    toElements' m = do
        r <- runStateT (runReactMlT m) mempty
        case r of
            (Nothing, _) -> pure Nothing
            (Just _, xs) -> Just <$> (liftIO $ sequenceA $ fromMarkup <$> D.toList xs)

-- | Fully render the ReactMlt into a [R.ReactElement]
markedElements :: MonadIO io => G.WindowT s (ReactMlT io) () -> s -> io [R.ReactElement]
markedElements w = (fmap (fromMaybe [])) <$> view G._WRMT' (markedWindow w)

-- | Fully render the ReactMlt into a R.ReactElement
markedElement :: MonadIO io => G.WindowT s (ReactMlT io) () -> s -> io R.ReactElement
markedElement w s = markedElements w s >>= liftIO . R.mkCombinedElements

#ifdef __GHCJS__

-- | Combine functions into a single function
-- Given two 'Callback (JSVal -> IO ())'
-- return a function that calls both callbacks
foreign import javascript unsafe
    "$r = function(j) { $1(j); $2(j); };"
    js_combineCallback1 :: JE.JSVar -> JE.JSVar -> JE.JSVar

#else

js_combineCallback1 :: JE.JSVar -> JE.JSVar -> JE.JSVar
js_combineCallback1 _ _ = JE.JSVar J.nullRef

#endif
