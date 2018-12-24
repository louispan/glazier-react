{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Glazier.React.Aeson where

import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as E
import qualified Data.Aeson.Types as A
import qualified Data.JSString as J
import GHC.Stack
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.Obj
import Glazier.React.Reactor

class FromModel s where
    mkEncoding :: (AsReactor c, MonadCommand c m) => s -> m A.Encoding

mkEntityEncoding :: (HasCallStack, FromModel s', AsReactor c, MonadCommand c m) => Entity o' s' -> m A.Encoding
mkEntityEncoding e = getModelOf e >>= mkEncoding

mkObjEncoding :: (HasCallStack, GetWeakObj o s', FromModel s', AsReactor c, MonadCommand c m) => o -> m A.Encoding
mkObjEncoding o = getModelOf e >>= mkEncoding
    where
        e = Entity id (weakObj o)

class ToModel s where
    parseModelJSON :: (AsReactor c, MonadCommand c m) => A.Value -> A.Parser (m s)

mkListEncoding :: Applicative m => (a -> m A.Encoding) -> [a] -> m A.Encoding
mkListEncoding _  [] = pure E.emptyArray_
mkListEncoding f (x:xs) = (pure E.openBracket) `combine` f x `combine` commas xs `combine` (pure E.closeBracket)
  where
    combine = liftA2 (E.><)
    commas = foldr (\v vs -> (pure E.comma) `combine` f v `combine` vs) (pure $ E.Encoding mempty)
{-# INLINE mkListEncoding #-}

instance FromModel a => FromModel [a] where
    mkEncoding = mkListEncoding mkEncoding

instance FromModel a => FromModel [Obj a] where
    mkEncoding = mkListEncoding mkObjEncoding

instance FromModel a => FromModel [WeakObj a] where
    mkEncoding = mkListEncoding mkObjEncoding

-----------------------------------------------------
-- FIXME: More instances, Maybe too

instance FromModel Bool where
    mkEncoding = pure . A.toEncoding

instance ToModel Bool where
    parseModelJSON = fmap pure . A.parseJSON

instance FromModel Int where
    mkEncoding = pure . A.toEncoding

instance ToModel Int where
    parseModelJSON = fmap pure . A.parseJSON

instance FromModel J.JSString where
    mkEncoding = pure . A.toEncoding

instance ToModel J.JSString where
    parseModelJSON = fmap pure . A.parseJSON

