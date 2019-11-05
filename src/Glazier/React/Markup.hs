{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Markup
    ( AskMarkup
    , askMarkup
    , PutMarkup
    , putMarkup
    , modifyMarkup
    , appendMarkup
    , ReactMarkup(..)
    , BranchParam(..)
    , LeafParam(..)
    , fromMarkup
    , fromElement
    , toElements
    , toElement
    , textMarkup
    , leafMarkup
    , branchMarkup
    ) where

import Control.Monad.Environ
import qualified Data.DList as DL
import Glazier.React.ReactElement
import JS.Data

type AskMarkup = MonadAsk' (DL.DList ReactMarkup)
askMarkup :: AskMarkup m => m (DL.DList ReactMarkup)
askMarkup = askEnv' @(DL.DList ReactMarkup)

type PutMarkup = MonadPut' (DL.DList ReactMarkup)
putMarkup :: PutMarkup m => DL.DList ReactMarkup -> m ()
putMarkup = putEnv' @(DL.DList ReactMarkup)

modifyMarkup :: PutMarkup m => (DL.DList ReactMarkup -> DL.DList ReactMarkup) -> m ()
modifyMarkup = modifyEnv' @(DL.DList ReactMarkup)

appendMarkup :: PutMarkup m => DL.DList ReactMarkup -> m ()
appendMarkup a = modifyMarkup (*> a)

-- | The parameters required to create a branch ReactElement with children
data BranchParam = BranchParam
    JSVal
    (DL.DList (JSString, JSVal))
    (DL.DList ReactMarkup)

-- | The parameters required to create a leaf ReactElement (no children)
data LeafParam = LeafParam
    JSVal
    (DL.DList (JSString, JSVal))

data ReactMarkup
    = ElementMarkup ReactElement
    | TextMarkup JSString
    | BranchMarkup BranchParam
    | LeafMarkup LeafParam

-- | Create 'ReactElement's from a 'ReactMarkup'
fromMarkup :: ReactMarkup -> IO ReactElement
fromMarkup (BranchMarkup (BranchParam n props xs)) = do
    xs' <- sequenceA $ fromMarkup <$> (DL.toList xs)
    mkBranchElement n (DL.toList props) xs'

fromMarkup (LeafMarkup (LeafParam n props)) =
    mkLeafElement n (DL.toList props)

fromMarkup (TextMarkup str) = pure $ rawTextElement str

fromMarkup (ElementMarkup e) = pure e

-------------------------------------------------

-- | To use an exisitng ReactElement
fromElement :: PutMarkup m => ReactElement -> m ()
fromElement e = modifyMarkup (`DL.snoc` ElementMarkup e)

-- | Convert the ReactMlt to [ReactElement]
toElements :: DL.DList ReactMarkup -> IO [ReactElement]
toElements xs = sequenceA $ fromMarkup <$> DL.toList xs

-- | 'Glazier.React.ReactDOM.renderDOM' only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div' if required.
-- If there is only one element in the list, then nothing is changed.
toElement :: DL.DList ReactMarkup -> IO ReactElement
toElement xs = toElements xs >>= mkCombinedElements

-------------------------------------------------

-- | For raw text content
textMarkup :: PutMarkup m => JSString -> m ()
textMarkup n = modifyMarkup (`DL.snoc` TextMarkup n)

-- | For the contentless elements: eg 'br_'.
-- Memonic: lf for leaf.
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
leafMarkup :: PutMarkup m
    => JSVal
    -> (DL.DList (JSString, JSVal))
    -> m ()
leafMarkup n props = modifyMarkup (`DL.snoc` LeafMarkup (LeafParam n props))

-- | Create a MonadState that run the given given a combining function
-- where the first arg is the state from running the markup producing MonadState with mempty,
-- and the 2nd arg the starting state of the resultant MonadState.
withMarkup :: PutMarkup m
    => (DL.DList ReactMarkup -> DL.DList ReactMarkup -> DL.DList ReactMarkup)
    -> m a
    -> m a
withMarkup f childs = do
    -- save state
    s <- askMarkup
    -- run children with mempty
    putMarkup mempty
    a <- childs
    childs' <- askMarkup
    -- restore state
    putMarkup s
    -- append the children markup
    modifyMarkup (f childs')
    pure a

-- | For the contentful elements: eg 'div_'.
-- Memonic: bh for branch.
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
branchMarkup :: PutMarkup m
    => JSVal
    -> (DL.DList (JSString, JSVal))
    -> m a
    -> m a
branchMarkup n props = withMarkup (\childs' ms -> ms `DL.snoc` BranchMarkup (BranchParam n props childs'))

-- -- Given a mapping function, apply it to children of the markup
-- modifyMarkup :: MonadState (DL.DList ReactMarkup) m
--     => (DL.DList ReactMarkup -> DL.DList ReactMarkup)
--     -> m a -> m a
-- modifyMarkup f = withMarkup (\childs' ms -> ms `DL.append` f childs')

-- -- Given a mapping function, apply it to all child BranchMarkup or LeafMarkup (if possible)
-- -- Does not recurse into decendants.
-- overSurfaceProperties ::
--     ((DL.DList JE.Property) -> (DL.DList JE.Property))
--     -> (DL.DList ReactMarkup -> DL.DList ReactMarkup)
-- overSurfaceProperties f childs = DL.fromList $ case DL.toList childs of
--     LeafMarkup (LeafParam j ps) : bs ->
--         LeafMarkup (LeafParam j (f ps)) : bs
--     BranchMarkup (BranchParam j ps as) : bs ->
--         BranchMarkup (BranchParam j (f ps) as) : bs
--     bs -> bs

-- -- Given a mapping function, apply it to all child BranchMarkup or LeafMarkup (if possible)
-- -- Recurse into decendants.
-- overAllProperties ::
--     ((DL.DList JE.Property) -> (DL.DList JE.Property))
--     -> (DL.DList ReactMarkup -> DL.DList ReactMarkup)
-- overAllProperties f childs = DL.fromList $ case DL.toList childs of
--     LeafMarkup (LeafParam j ps) : bs ->
--         LeafMarkup (LeafParam j (f ps)) : bs
--     BranchMarkup (BranchParam j ps as) : bs ->
--         BranchMarkup (BranchParam j (f ps) (overAllProperties f as)) : bs
--     bs -> bs
