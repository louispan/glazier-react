{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | A more generic form of 'Lucid.Term' and With from 'Lucid.With' by using
-- extra type parameters for Text and [Attribute]
module Glazier.React.DOM where

-- import GHCJS.Marshal.Pure (PToJSVal(..))
-- import GHCJS.Types (IsJSVal, JSString, JSVal, jsval)
-- import JavaScript.Array (JSArray, fromList)


-- import qualified Data.Text as T

-- -- | With an element use these attributes. An overloaded way of adding
-- -- attributes either to an element accepting attributes-and-children
-- -- or one that just accepts attributes. See the two instances.
-- class With attrs a where
--   -- | With the given element(s), use the given attributes.
--   with :: a -- ^ Some element, either @Html a@ or @Html a -> Html a@.
--        -> attrs
--        -> a

-- instance Monad m => With [L.Attribute] (L.HtmlT m a) where
--     with = L.with

-- instance Monad m => With [L.Attribute] (L.HtmlT m a -> L.HtmlT m a) where
--     with = L.with

-- -- | Used to construct HTML terms.
-- --
-- -- Simplest use: p_ = term "p" yields 'Lucid.Html5.p_'.
-- --
-- -- Very overloaded for three cases:
-- --
-- -- * The first case is the basic @arg@ of @[(Text,Text)]@ which will
-- --   return a function that wants children.
-- -- * The second is an @arg@ which is @HtmlT m ()@, in which case the
-- --   term accepts no attributes and just the children are used for the
-- --   element.
-- -- * Finally, this is also used for overloaded attributes, like
-- --   `Lucid.Html5.style_` or `Lucid.Html5.title_`. If a return type of @(Text,Text)@ is inferred
-- --   then an attribute will be made.
-- --
-- -- The instances look intimidating but actually the constraints make
-- -- it very general so that type inference works well even in the
-- -- presence of things like @OverloadedLists@ and such.
-- class Term txt attrs arg result | result -> txt attrs arg where
--   -- | Used for constructing elements e.g. @term "p"@ yields 'Lucid.Html5.p_'.
--   term :: txt      -- ^ Name of the element or attribute.
--        -> arg      -- ^ Either an attribute list or children.
--        -> result   -- ^ Result: either an element or an attribute.

--   -- | Use this if you want to make an element which inserts some
--   -- pre-prepared attributes into the element.
--   termWith :: txt           -- ^ Name.
--            -> attrs         -- ^ Attribute transformer.
--            -> arg           -- ^ Some argument.
--            -> result        -- ^ Result: either an element or an attribute.

-- -- | Given attributes, expect more child input.
-- instance (Monad m, f ~ L.HtmlT m a) => Term T.Text [L.Attribute] [L.Attribute] (f -> L.HtmlT m a) where
--     term = L.term
--     termWith = L.termWith

-- -- | Given children immediately, just use that and expect no
-- -- attributes.
-- instance (Monad m) => Term T.Text [L.Attribute] (L.HtmlT m a) (L.HtmlT m a) where
--     term = L.term
--     termWith = L.termWith

-- -- | Some terms (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- -- attributes as well as elements.
-- instance Term T.Text [L.Attribute] T.Text L.Attribute where
--     term = L.term
--     termWith = L.termWith
