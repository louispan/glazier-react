{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Markup
    ( ReactMarkup(..)
    , BranchParam(..)
    , LeafParam(..)
    , fromMarkup
    , fromElement
    , toElements
    , toElement
    , txt
    , leaf
    , branch
    , withMarkup
    , modifyMarkup
    , overSurfaceProperties
    , modifySurfaceProperties
    ) where

import Control.Monad.State.Strict
import qualified Data.DList as DL
import qualified GHCJS.Types as J
import qualified Glazier.React.Element as Z
import qualified JavaScript.Extras as JE

-- | The parameters required to create a branch ReactElement with children
data BranchParam = BranchParam
    JE.JSRep
    (DL.DList JE.Property)
    [ReactMarkup]

-- | The parameters required to create a leaf ReactElement (no children)
data LeafParam = LeafParam
    JE.JSRep
    (DL.DList JE.Property)

data ReactMarkup
    = ElementMarkup Z.ReactElement
    | TextMarkup J.JSString
    | BranchMarkup BranchParam
    | LeafMarkup LeafParam

-- | Create 'ReactElement's from a 'ReactMarkup'
fromMarkup :: ReactMarkup -> IO Z.ReactElement
fromMarkup (BranchMarkup (BranchParam n props xs)) = do
    xs' <- sequenceA $ fromMarkup <$> xs
    Z.mkBranchElement n (DL.toList props) xs'

fromMarkup (LeafMarkup (LeafParam n props)) =
    Z.mkLeafElement n (DL.toList props)

fromMarkup (TextMarkup str) = pure $ Z.textElement str

fromMarkup (ElementMarkup e) = pure e

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
    => n
    -> (DL.DList JE.Property)
    -> m ()
leaf n props = modify' (`DL.snoc` LeafMarkup (LeafParam (JE.toJSR n) props))

-- | Create a MonadState that run the given given a combining function and markup producing MonadState.
withMarkup :: MonadState (DL.DList ReactMarkup) m
    => (DL.DList ReactMarkup -> DL.DList ReactMarkup -> DL.DList ReactMarkup)
    -> m a
    -> m a
withMarkup f childs = do
    -- save state
    s <- get
    -- run children with mempty
    put mempty
    a <- childs
    childs' <- get
    -- restore state
    put s
    modify' (f childs')
    pure a

-- | For the contentful elements: eg 'div_'
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
-- Listeners are more important than properties so they will be rendered
-- after properties so they do not get overridden.
branch :: (JE.ToJS n, MonadState (DL.DList ReactMarkup) m)
    => n
    -> (DL.DList JE.Property)
    -> m a
    -> m a
branch n props = withMarkup (\childs' ms -> ms `DL.snoc` BranchMarkup (BranchParam (JE.toJSR n) props (DL.toList childs')))

-- Given a mapping function, apply it to children of the markup
modifyMarkup :: MonadState (DL.DList ReactMarkup) m
    => (DL.DList ReactMarkup -> DL.DList ReactMarkup)
    -> m a -> m a
modifyMarkup f = withMarkup (\childs' ms -> ms `DL.append` f childs')

-- Given a mapping function, apply it to all child BranchMarkup or LeafMarkup (if possible)
-- Does not recurse into decendants.
overSurfaceProperties ::
    (DL.DList JE.Property -> DL.DList JE.Property)
    -> (DL.DList ReactMarkup -> DL.DList ReactMarkup)
overSurfaceProperties f childs = DL.fromList $ case DL.toList childs of
    LeafMarkup (LeafParam j ps) : bs ->
        LeafMarkup (LeafParam j (f ps)) : bs
    BranchMarkup (BranchParam j ps as) : bs ->
        BranchMarkup (BranchParam j (f ps) as) : bs
    bs -> bs

modifySurfaceProperties :: MonadState (DL.DList ReactMarkup) m
    => (DL.DList JE.Property -> DL.DList JE.Property)
    -> m a -> m a
modifySurfaceProperties f = modifyMarkup (overSurfaceProperties f)

