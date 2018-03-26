{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Markup
    ( Listener
    , ReactMarkup(..)
    , BranchParam(..)
    , LeafParam(..)
    , fromMarkup
    , fromElement
    , toElements
    , toElement
    , txt
    , leaf
    , lf
    , branch
    , bh
    , modifyMarkup
    , overSurfaceProperties
    , modifySurfaceProperties
    ) where

import Control.Monad.State.Strict
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Element as Z
import qualified JavaScript.Extras as JE

type Listener = (J.JSString, J.Callback (J.JSVal -> IO ()))

-- | The parameters required to create a branch ReactElement with children
data BranchParam = BranchParam
    (DL.DList Listener)
    JE.JSRep
    (DL.DList JE.Property)
    [ReactMarkup]

-- | The parameters required to create a leaf ReactElement (no children)
data LeafParam = LeafParam
    (DL.DList Listener)
    JE.JSRep
    (DL.DList JE.Property)

data ReactMarkup
    = ElementMarkup Z.ReactElement
    | TextMarkup J.JSString
    | BranchMarkup BranchParam
    | LeafMarkup LeafParam

-- | Create 'ReactElement's from a 'ReactMarkup'
fromMarkup :: ReactMarkup -> IO Z.ReactElement
fromMarkup (BranchMarkup (BranchParam ls n props xs)) = do
    xs' <- sequenceA $ fromMarkup <$> xs
    Z.mkBranchElement n (DL.toList props <> dedupListeners (DL.toList ls)) xs'

fromMarkup (LeafMarkup (LeafParam ls n props)) =
    Z.mkLeafElement n (DL.toList props <> dedupListeners (DL.toList ls))

fromMarkup (TextMarkup str) = pure $ Z.textElement str

fromMarkup (ElementMarkup e) = pure e

-- -- | Monadic generator of ReactMarkup.
-- -- It is a CPS-style WriterT (ie a StateT) to build up a function to
-- -- build up a computations to generate a '[ReactMarkup]'.
-- -- You can use 'runStateT' with an initial state of 'mempty'.
-- newtype ReactMlT m a = ReactMlT
--     { runReactMlT :: StateT (DL.DList ReactMarkup) m a
--     } deriving ( MonadState (DL.DList ReactMarkup)
--                , Monad
--                , Applicative
--                , Functor
--                , Fail.MonadFail
--                , Alternative
--                , MonadPlus
--                , MonadFix
--                , MonadIO
--                , MFunctor
--                , G.Generic
--                )

-- type ReactMl = ReactMlT Identity

-- instance MonadTrans ReactMlT where
--     lift = ReactMlT . lift

-- instance (Semigroup a, Monad m) => Semigroup (ReactMlT m a) where
--     (<>) = liftA2 (<>)

-- instance (Monoid a, Monad m) => Monoid (ReactMlT m a) where
--     mempty = pure mempty
--     mappend = liftA2 mappend

-------------------------------------------------

-- | To use an exisitng ReactElement
fromElement :: MonadState (DL.DList ReactMarkup) m => Z.ReactElement -> m ()
fromElement e = modify' (`DL.snoc` ElementMarkup e)

-- | Convert the ReactMlt to [Z.ReactElement]
toElements :: DL.DList ReactMarkup -> IO [Z.ReactElement]
toElements xs = sequenceA $ fromMarkup <$> DL.toList xs

-- | Fully render the ReactMlt into a single Z.ReactElement
toElement :: DL.DList ReactMarkup -> IO Z.ReactElement
toElement xs = toElements xs >>= Z.mkCombinedElements

-- -- | toElements reading an s from the environment
-- toElements' :: MonadIO io => (s -> ReactMlT io ()) -> s -> io [Z.ReactElement]
-- toElements' f = (toElements . f)

-------------------------------------------------

-- | For text content
txt :: MonadState (DL.DList ReactMarkup) m => J.JSString -> m ()
txt n = modify' (`DL.snoc` TextMarkup n)

-- | For the contentless elements: eg 'br_'
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
-- Listeners are more important than properties so they will be rendered
-- after properties so they do not get overridden.
leaf :: (JE.ToJS n, MonadState (DL.DList ReactMarkup) m)
    => (DL.DList Listener)
    -> n
    -> (DL.DList JE.Property)
    -> m ()
leaf ls n props = modify' (`DL.snoc` LeafMarkup (LeafParam ls (JE.toJSR n) props))

-- | Convenient version of 'leaf' without listeners
-- Memenoic: short for 'leaf'
lf
    :: (JE.ToJS n, MonadState (DL.DList ReactMarkup) m)
    => n
    -> (DL.DList JE.Property)
    -> m ()
lf = leaf []

-- | For the contentful elements: eg 'div_'
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
-- Listeners are more important than properties so they will be rendered
-- after properties so they do not get overridden.
branch :: (JE.ToJS n, MonadState (DL.DList ReactMarkup) m)
    => (DL.DList Listener)
    -> n
    -> (DL.DList JE.Property)
    -> m a
    -> m a
branch ls n props childs = do
    -- save state
    s <- get
    -- run children with mempty
    put mempty
    a <- childs
    childs' <- get
    -- restore state
    put s
    modify' (`DL.snoc` BranchMarkup (BranchParam ls (JE.toJSR n) props (DL.toList childs')))
    pure a

-- | Convenient version of 'branch' without listeners
-- Memenoic: short for 'branch'
bh
    :: (JE.ToJS n, MonadState (DL.DList ReactMarkup) m)
    => n
    -> (DL.DList JE.Property)
    -> m a
    -> m a
bh = branch []

-- | dedups a list of (key, Callback1) by merging callbacks for the same key together.
dedupListeners :: [Listener] -> [JE.Property]
dedupListeners = M.toList . M.fromListWith js_combineCallback1 . fmap (fmap JE.toJSR)

-- Given a mapping function, apply it to children of the markup
modifyMarkup :: MonadState (DL.DList ReactMarkup) m
    => (DL.DList ReactMarkup -> DL.DList ReactMarkup)
    -> m a -> m a
modifyMarkup f childs = do
    -- save state
    s <- get
    -- run children with mempty
    put mempty
    a <- childs
    childs' <- get
    -- restore state
    put s
    modify' (`DL.append` f childs')
    pure a

-- Given a mapping function, apply it to all child BranchMarkup or LeafMarkup (if possible)
-- Does not recurse into decendants.
overSurfaceProperties ::
    (DL.DList JE.Property -> DL.DList JE.Property)
    -> (DL.DList ReactMarkup -> DL.DList ReactMarkup)
overSurfaceProperties f childs = DL.fromList $ case DL.toList childs of
    LeafMarkup (LeafParam ls j ps) : bs ->
        LeafMarkup (LeafParam ls j (f ps)) : bs
    BranchMarkup (BranchParam ls j ps as) : bs ->
        BranchMarkup (BranchParam ls j (f ps) as) : bs
    bs -> bs

modifySurfaceProperties :: MonadState (DL.DList ReactMarkup) m
    => (DL.DList JE.Property -> DL.DList JE.Property)
    -> m a -> m a
modifySurfaceProperties f = modifyMarkup (overSurfaceProperties f)

#ifdef __GHCJS__

-- | Combine functions into a single function
-- Given two 'Callback (JSVal -> IO ())'
-- return a function that calls both callbacks
foreign import javascript unsafe
    "$r = function(j) { $1(j); $2(j); };"
    js_combineCallback1 :: JE.JSRep -> JE.JSRep -> JE.JSRep

#else

js_combineCallback1 :: JE.JSRep -> JE.JSRep -> JE.JSRep
js_combineCallback1 _ _ = JE.JSRep J.nullRef

#endif
