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
    ( Markup
    , ReactMarkup(..)
    , BranchParam(..)
    , LeafParam(..)
    , fromMarkup
    , toElements
    , toElement
    , elementMarkup
    , textMarkup
    , leafMarkup
    , branchMarkup
    ) where

import Control.Monad.Environ
import qualified Data.DList as DL
import Glazier.React.ReactElement
import JS.Data

type Markup = DL.DList ReactMarkup

-- | The parameters required to create a branch ReactElement with children
data BranchParam = BranchParam
    JSVal
    [(JSString, JSVal)]
    [ReactMarkup]

-- | The parameters required to create a leaf ReactElement (no children)
data LeafParam = LeafParam
    JSVal
    [(JSString, JSVal)]

data ReactMarkup
    = ElementMarkup ReactElement
    | TextMarkup JSString
    | BranchMarkup BranchParam
    | LeafMarkup LeafParam

-- | Create 'ReactElement's from a 'ReactMarkup'
fromMarkup :: ReactMarkup -> IO ReactElement
fromMarkup (BranchMarkup (BranchParam n props xs)) = do
    xs' <- sequenceA $ fromMarkup <$> xs
    mkBranchElement n props xs'

fromMarkup (LeafMarkup (LeafParam n props)) =
    mkLeafElement n props

fromMarkup (TextMarkup str) = pure $ rawTextElement str

fromMarkup (ElementMarkup e) = pure e

-------------------------------------------------

-- | Convert the ReactMlt to [ReactElement]
toElements :: [ReactMarkup] -> IO [ReactElement]
toElements xs = sequenceA $ fromMarkup <$> xs

-- | 'Glazier.React.ReactDOM.renderDOM' only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div' if required.
-- If there is only one element in the list, then nothing is changed.
toElement :: [ReactMarkup] -> IO ReactElement
toElement xs = toElements xs >>= mkCombinedElements

-------------------------------------------------

-- | To use an exisitng ReactElement
elementMarkup :: MonadPut' Markup m => ReactElement -> m ()
elementMarkup e = modifyEnv' @Markup (`DL.snoc` ElementMarkup e)

-- | For raw text content
textMarkup :: MonadPut' Markup m => JSString -> m ()
textMarkup n = modifyEnv' @Markup (`DL.snoc` TextMarkup n)

-- | For the contentless elements: eg 'br_'.
-- Memonic: lf for leaf.
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
leafMarkup :: MonadPut' Markup m
    => JSVal
    -> [(JSString, JSVal)]
    -> m ()
leafMarkup n props = modifyEnv' @Markup (`DL.snoc` LeafMarkup (LeafParam n props))

-- | For the contentful elements: eg 'div_'.
-- Memonic: bh for branch.
-- Duplicate listeners with the same key will be combined, but it is a silent error
-- if the same key is used across listeners and props.
-- "If an attribute/prop is duplicated the last one defined wins."
-- https://www.reactenlightenment.com/react-nodes/4.4.html
branchMarkup :: MonadPut' Markup m
    => JSVal
    -> [(JSString, JSVal)]
    -> m a
    -> m a
branchMarkup n props child = do
    -- save state
    s <- getEnv' @Markup
    -- run children with mempty
    putEnv' @Markup mempty
    a <- child
    child' <- getEnv' @Markup
    -- restore state
    putEnv' @Markup s
    -- append the children markup
    modifyEnv' @Markup (`DL.snoc` BranchMarkup (BranchParam n props (DL.toList child')))
    pure a

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
