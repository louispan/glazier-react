{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Lucid.HtmlT' inspired monad for creating 'ReactElement's
module Glazier.React.Element
    ( ReactElement
    , unsafeCoerceReactElement
    , createReactElement
    , stringReactElement
    , combineReactElements
    , ReactMarkup(..)
    , fromReactMarkup
    , fromReactMarkups
    , ReactMlT(..)
    , ReactMl
    ) where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Types (IsJSVal, JSString, JSVal, jsval)
import Glazier.React.Internal (PureJSVal(..))
import JavaScript.Array (JSArray, fromList)
import JavaScript.Object (Object, create, unsafeSetProp)

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
createReactElement n props xs = do
    props' <- toJSProps props
    js_createReactElement n (pToJSVal (PureJSVal <$> props')) (fromList $ jsval <$> xs)

foreign import javascript unsafe
    "$r = $1;"
    js_stringReactElement :: JSString -> ReactElement

-- | Not an IO action because JSString is immutable
stringReactElement :: JSString -> ReactElement
stringReactElement = js_stringReactElement

-- | Wrap a list of ReactElements inside a 'div'
foreign import javascript unsafe
    "$r = hgr$combineElements($1, $2);"
    js_combineElements :: JSVal -> JSArray -> IO ReactElement

-- | React only allows a single top most element.
-- Provide a handly function to wrap a list of ReactElements inside a 'div'
combineReactElements :: M.HashMap JSString JSVal -> [ReactElement] -> IO ReactElement
combineReactElements props xs = do
    props' <- toJSProps props
    js_combineElements (pToJSVal (PureJSVal <$> props')) (fromList $ jsval <$> xs)

-- | The parameters required to create a ReactElement
data ReactMarkup = ReactString JSString | ReactAtom
    { name :: JSString
    , properties :: M.HashMap JSString JSVal
    , children :: [ReactMarkup]
    }

-- | Create 'ReactElement's from a 'ReactAtom'
fromReactMarkup :: ReactMarkup -> IO (ReactElement)
fromReactMarkup (ReactAtom n p xs) = do
    xs' <- sequenceA $ fromReactMarkup <$> xs
    createReactElement n p xs'

fromReactMarkup (ReactString str) = pure $ stringReactElement str

-- | Create a single 'ReactElement' from '[ReactMark]'
fromReactMarkups :: M.HashMap JSString JSVal -> [ReactMarkup] -> IO (ReactElement)
fromReactMarkups props xs = do
    xs' <- sequenceA $ fromReactMarkup <$> xs
    combineReactElements props xs'

-- | Monadic generator of ReactActom.
-- It is a CPS-style WriterT (ie a StateT) to build up a function
-- build up a computations to generate a '[ReactAtom]'.
-- You can use 'runStateT' with an initial state of 'mempty'.
newtype ReactMlT m a = ReactMlT
    { runReactMarkupT :: StateT (M.HashMap JSString JSVal -> [ReactMarkup]) m a
    } deriving ( MonadState (M.HashMap JSString JSVal -> [ReactMarkup])
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

makeWrapped ''ReactMlT

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
