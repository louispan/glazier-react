{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Element
    ( ReactElement
    , unsafeCoerceReactElement
    , createReactElement
    , combineReactElements
    ) where

import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Types (IsJSVal(..), JSString, JSVal, jsval, nullRef)
import JavaScript.Array (JSArray, fromList)
import JavaScript.Object (Object, create, unsafeSetProp)
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
import qualified Data.HashMap.Strict as M

-- FIXME: How to enable foreign ReactElements?
newtype ReactElement = ReactElement JSVal
instance IsJSVal ReactElement
instance PToJSVal ReactElement where
    pToJSVal = jsval

-- | This is an IO action even if the same args was used
-- a different ReactElement may be created, because JSVal
-- and JSArray are mutable.
foreign import javascript unsafe
    "$r = React.createElement($1, $2, $3);"
    js_createReactElement :: JSString -> JSVal -> JSArray -> IO ReactElement

-- | Unfortunately, ReactJS did not export an easy way to check if something is a ReactElement,
-- although they do so in the internal code with REACT_ELEMENT_TYPE.
-- This function allow coercing a ReactElement from a JSVal
-- and is marked unsafe as a reminder that the coersion is unchecked.
-- This function is required when receiving ReactElement from javascript (eg in a callback)
-- or to interface with foreign React Elements.
unsafeCoerceReactElement :: JSVal -> ReactElement
unsafeCoerceReactElement = ReactElement

-- | Create a JS Object from a HashMap
toJSProps :: M.HashMap JSString JSVal -> IO (Maybe Object)
toJSProps m | M.null m = pure Nothing
toJSProps m | otherwise = do
                    obj <- create
                    M.foldlWithKey' (\action k v -> action >> unsafeSetProp k v obj) (pure ()) m
                    pure (Just obj)

-- | Create a react from a HashMap of properties
createReactElement :: JSString -> M.HashMap JSString JSVal -> [ReactElement] -> IO ReactElement
createReactElement name props xs = do
    props' <- toJSProps props
    js_createReactElement name (pToJSVal (PureJSVal <$> props')) (fromList $ jsval <$> xs)

foreign import javascript unsafe
    "$r = hgr$combineElements($1, $2);"
    js_combineElements :: JSVal -> JSArray -> IO ReactElement

-- | React only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div'
combineReactElements :: M.HashMap JSString JSVal -> [ReactElement] -> IO ReactElement
combineReactElements props xs = do
    props' <- toJSProps props
    js_combineElements (pToJSVal (PureJSVal <$> props')) (fromList $ jsval <$> xs)

-- | The pure bits required to create a ReactElement
data PureReactElement = PureReactElement
    { name :: JSString
    , props :: M.HashMap JSString JSVal
    , children :: [PureReactElement]
    }

-- -- | Create 'ReactElement's from a ReactElementParam
-- mkReactElement :: PureReactElement -> IO (ReactElement)
-- mkReactElement (PureReactElement n p xs) = do
--     xs' <- sequenceA $ mkReactElement <$> xs
--     createElement' n p xs'

-- -- | Create a single 'ReactELement' from [ReactElementParam]
-- mkReactElements :: Maybe Object -> [PureReactElement] -> IO (ReactElement)
-- mkReactElements props xs = combineElements props <$> (sequenceA $ mkReactElement <$> xs)

-- -- | Monadic generator of ReactMarkup
-- -- This is the low-level way to run the ReactElement transformer,
-- -- finally returning the parts to create a list of PureReactElement.
-- -- It is a CPS-style WriterT (ie a StateT) of a function that outputs [PureReactElement]
-- -- You can use 'runStateT' with an initial state of 'mempty'.
-- newtype ReactMarkupT m a = ReactMarkupT
--     { runReactMarkupT :: StateT (M.HashMap JSString JSVal -> [PureReactElement]) m a
--     } deriving ( MonadState (M.HashMap JSString JSVal -> [PureReactElement])
--                , Monad
--                , Applicative
--                , Functor
--                , Fail.MonadFail
--                , Alternative
--                , MonadPlus
--                , MonadFix
--                , MonadIO
--                )

-- type ReactMarkup = ReactMarkupT Identity

-- makeWrapped ''ReactMarkupT

-- wack :: String -> JSVal
-- wack =

    
-- -- -- | Build the React markup. Analogous to @execState@.
-- execReactMarkupT
--     :: Monad m
--     => ReactMarkupT m a -> m ReactElement -- ^ The @a@ is discarded.
-- execReactMarkupT (ReactMarkupT m) = do
--     (f, _) <- runReactMarkupT m
--     return (f (M.empty, mempty))

-- -- | Evaluate the ReactMarkup to its return value. Analogous to @evalState@.
-- --
-- -- Use this if you want to ignore the ReactElement output of an action
-- -- completely and just get the result.
-- evalHtmlT :: Monad m => ReactMarkupT m a
--           -> m a       -- ^ Ignore the ReactElement output and just return the value.
-- evalHtmlT m = do
--     (_, a) <- runReactMarkupT m
--     return a

-- newtype ReactMarkup a = ReactMarkupT {
--     runReactMarkupT :: ReaderT (M.HashMap JSString JSVal, [ReactElement]) (StateT ReactElement m) a
--     } deriving ( Monad
--                , Applicative
--                , Functor
--                , Fail.MonadFail
--                , Alternative
--                , MonadPlus
--                , MonadFix
--                , MonadIO
--                )
