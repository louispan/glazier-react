{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Window here refers to 'Glazier.Window' (ie. rendering View of state), not browser window
module Glazier.React.Element
    ( ReactElement
    , unsafeCoerceReactElement
    , createElement
    , combineElements
    ) where

import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Types (IsJSVal(..), JSString, JSVal, jsval)
import JavaScript.Array (JSArray, fromList)
import JavaScript.Object (Object)
import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Profunctor
import Data.Semigroup
import Glazier.Class
import Glazier.React.Internal (PureJSVal(..))

-- FIXME: How to enable foreign ReactElements?
newtype ReactElement = ReactElement JSVal
instance IsJSVal ReactElement
instance PToJSVal ReactElement where
    pToJSVal = jsval

foreign import javascript unsafe
  "$r = React.createElement($1, $2, $3);"
  js_createElement :: JSString -> JSVal -> JSArray -> ReactElement

-- |
-- The constructor for ReactElement is exported for convenient interoperabilithy with foreign
-- react components. It is assumed that it is only constructed for JSVal that have been created
-- using `Glazier.React.createElement` or equivalent.
-- Unfortunately, ReactJS did not export an easy way to check if something is a ReactElement,
-- although they do so in the internal code with REACT_ELEMENT_TYPE.
-- This function allow coercing a ReactElement from a JSVal
-- and is marked unsafe as a reminder that the coersion is unchecked.
-- This function is required when receiving ReactElement from javascript (eg in a callback)
-- or to interface with foreign React Elements.
unsafeCoerceReactElement :: JSVal -> ReactElement
unsafeCoerceReactElement = ReactElement

createElement :: JSString -> Maybe Object -> [ReactElement] -> ReactElement
createElement name props xs = js_createElement name (pToJSVal (PureJSVal <$> props)) (fromList $ jsval <$> xs)

foreign import javascript unsafe
  "$r = hgr$combineElements($1, $2);"
  js_combineElements :: JSVal -> JSArray -> ReactElement

-- | React only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div'
combineElements :: Maybe Object -> [ReactElement] -> ReactElement
combineElements props xs = js_combineElements (pToJSVal (PureJSVal <$> props)) (fromList $ jsval <$> xs)

-- | Merge a final input of props and children into this function's captured state
-- and output the created ReactElement and a value.
-- This is equivalent to ReaderT (Object, [ReactElement]) (WriterT ReactElement m) a
-- WriterT is not strict enough, so we'll going to use StateT instead.
-- This is the low-level way to run the ReactElement transformer,
-- finally returning a ReactElement and a value. You can
-- pass (nullRef, mempty) for this argument for a top-level call.
-- newtype ReactElementT m a = ReactElementT {runReactElementT :: (Object, [ReactElement]) -> m (ReactElement, a)}
-- newtype ReactElementT m a = ReactElementT {runReactElementT :: (Object, [ReactElement]) -> m (ReactElement, a)}

newtype ReactMarkupT m a = ReactMarkupT {
    runReactMarkupT :: ReaderT (Object, [ReactElement]) (StateT ReactElement m) a
    } deriving ( Monad
               , Applicative
               , Functor
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               )
