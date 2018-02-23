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
    , toElement
    , txt
    , leaf
    , branch
    , modifyMarkup
    , overChildrenProperties
    ) where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
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
fromMarkup :: ReactMarkup -> IO R.ReactElement
fromMarkup (BranchMarkup (BranchParam n ls props xs)) = do
    xs' <- sequenceA $ fromMarkup <$> xs
    R.mkBranchElement n (props <> dedupListeners ls) xs'

fromMarkup (LeafMarkup (LeafParam n ls props)) = R.mkLeafElement n (props <> dedupListeners ls)

fromMarkup (TextMarkup str) = pure $ R.textElement str

fromMarkup (ElementMarkup e) = pure e

-- | Monadic generator of ReactMarkup.
-- It is a CPS-style WriterT (ie a StateT) to build up a function to
-- build up a computations to generate a '[ReactMarkup]'.
-- You can use 'runStateT' with an initial state of 'mempty'.
newtype ReactMlT m a = ReactMlT
    { runReactMlT :: StateT (DL.DList ReactMarkup) m a
    } deriving ( MonadState (DL.DList ReactMarkup)
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

-------------------------------------------------

-- | To use an exisitng ReactElement
fromElement :: Applicative m => R.ReactElement -> ReactMlT m ()
fromElement e = ReactMlT . StateT $ \xs -> pure ((), xs `DL.snoc` ElementMarkup e)

-- | Convert the ReactMlt to [R.ReactElement]
toElements :: MonadIO io => ReactMlT io () -> io [R.ReactElement]
toElements m = do
    xs <- execStateT (runReactMlT m) mempty
    liftIO $ sequenceA $ fromMarkup <$> DL.toList xs

-- | Fully render the ReactMlt into a R.ReactElement
toElement :: MonadIO io => ReactMlT io () -> io R.ReactElement
toElement m = toElements m >>= (liftIO . R.mkCombinedElements)

-- -- | toElements reading an s from the environment
-- toElements' :: MonadIO io => (s -> ReactMlT io ()) -> s -> io [R.ReactElement]
-- toElements' f = (toElements . f)

-------------------------------------------------

-- | For text content
txt :: Applicative m => J.JSString -> ReactMlT m ()
txt n = ReactMlT . StateT $ \xs -> pure ((), xs `DL.snoc` TextMarkup n)

-- | For the contentless elements: eg 'br_'
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
-- Listeners are more important than properties so they will be rendered
-- after properties so they do not get overridden.
leaf
    :: Monad m
    => JE.JSVar
    -> [Listener]
    -> [JE.Property]
    -> ReactMlT m ()
leaf n ls props = ReactMlT . StateT $ \xs -> pure ((), xs `DL.snoc` LeafMarkup (LeafParam n ls props))

-- | For the contentful elements: eg 'div_'
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
-- Listeners are more important than properties so they will be rendered
-- after properties so they do not get overridden.
branch
    :: Monad m
    => JE.JSVar
    -> [Listener]
    -> [JE.Property]
    -> ReactMlT m a
    -> ReactMlT m a
branch n ls props (ReactMlT (StateT childs)) = ReactMlT . StateT $ \xs -> do
    (a, childs') <- childs mempty
    pure (a, xs `DL.snoc` BranchMarkup (BranchParam n ls props (DL.toList childs')))

-- | dedups a list of (key, Callback1) by merging callbacks for the same key together.
dedupListeners :: [Listener] -> [JE.Property]
dedupListeners = M.toList . M.fromListWith js_combineCallback1 . fmap (fmap JE.toJS')

-- Given a mapping function, apply it to children of the markup
modifyMarkup :: Monad m
    => (DL.DList ReactMarkup -> DL.DList ReactMarkup)
    -> ReactMlT m a -> ReactMlT m a
modifyMarkup f (ReactMlT (StateT childs)) = ReactMlT . StateT $ \xs -> do
    (a, childs') <- childs mempty
    pure (a, xs `DL.append` f childs')

-- Given a mapping function, apply it to all child BranchMarkup or LeafMarkup (if possible)
-- Does not recurse into decendants.
overChildrenProperties ::
    ([JE.Property] -> [JE.Property])
    -> (DL.DList ReactMarkup -> DL.DList ReactMarkup)
overChildrenProperties f childs = DL.fromList $ case DL.toList childs of
    LeafMarkup (LeafParam j ls ps) : bs ->
        LeafMarkup (LeafParam j ls (f ps)) : bs
    BranchMarkup (BranchParam j ls ps as) : bs ->
        BranchMarkup (BranchParam j ls (f ps) as) : bs
    bs -> bs

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
