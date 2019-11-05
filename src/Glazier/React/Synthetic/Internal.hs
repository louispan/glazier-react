{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Synthetic.Internal
( SyntheticEvent(..)
, SyntheticUIEvent(..)
, SyntheticMouseEvent(..)
, SyntheticKeyboardEvent(..)
) where

import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM

-- | https://reactjs.org/docs/events.html
-- Every event in React is a synthetic event, a cross-browser wrapper around the native event.
-- which reused from a pool.
-- So it is dangerous to keep a reference to a 'SyntheticEvent' since it may expire and contain
-- other things without you knowing.
-- All relevant data from the 'SyntheticEvent' must be consumed as soon you get one.
-- That is, 'SyntheticEvent' must only be used in the first strict part of 'JS.Reactant.React.mkHandler.
-- It is not an instance of NFData and so cannot be returned into the second lazy part of 'JS.Reactant.React.mkHandler'
newtype SyntheticEvent = SyntheticEvent JSVal
    deriving (G.Generic, Show, ToJS, IsString)

instance FromJS SyntheticEvent where
    validFromJS = js_isSyntheticEvent
    fromJS a | js_isSyntheticEvent a = Just $ SyntheticEvent a
    fromJS _ = Nothing

instance IObject SyntheticEvent
instance IEvent SyntheticEvent

newtype SyntheticUIEvent = SyntheticUIEvent JSVal
    deriving (G.Generic, Show, ToJS, IsString)

instance FromJS SyntheticUIEvent where
    validFromJS = js_isSyntheticUIEvent
    fromJS a | js_isSyntheticUIEvent a = Just $ SyntheticUIEvent a
    fromJS _ = Nothing

instance IObject SyntheticUIEvent
instance IEvent SyntheticUIEvent
instance IUIEvent SyntheticUIEvent

newtype SyntheticMouseEvent = SyntheticMouseEvent JSVal
    deriving (G.Generic, Show, ToJS, IsString)

instance FromJS SyntheticMouseEvent where
    validFromJS = js_isSyntheticMouseEvent
    fromJS a | js_isSyntheticMouseEvent a = Just $ SyntheticMouseEvent a
    fromJS _ = Nothing

instance IObject SyntheticMouseEvent
instance IEvent SyntheticMouseEvent
instance IUIEvent SyntheticMouseEvent
instance ICommonKeyboardEvent SyntheticMouseEvent
instance IMouseEvent SyntheticMouseEvent

newtype SyntheticKeyboardEvent = SyntheticKeyboardEvent JSVal
    deriving (G.Generic, Show, ToJS, IsString)

instance FromJS SyntheticKeyboardEvent where
    validFromJS = js_isSyntheticKeyboardEvent
    fromJS a | js_isSyntheticKeyboardEvent a = Just $ SyntheticKeyboardEvent a
    fromJS _ = Nothing

instance IObject SyntheticKeyboardEvent
instance IEvent SyntheticKeyboardEvent
instance IUIEvent SyntheticKeyboardEvent
instance ICommonKeyboardEvent SyntheticKeyboardEvent
instance IKeyboardEvent SyntheticKeyboardEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Object && $1.nativeEvent instanceof Event"
    js_isSyntheticEvent :: JSVal -> Bool

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Object && $1.nativeEvent instanceof UIEvent"
    js_isSyntheticUIEvent :: JSVal -> Bool

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Object && $1.nativeEvent instanceof MouseEvent"
    js_isSyntheticMouseEvent :: JSVal -> Bool

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Object && $1.nativeEvent instanceof KeyboardEvent"
    js_isSyntheticKeyboardEvent :: JSVal -> Bool

#else

js_isSyntheticEvent :: JSVal -> Bool
js_isSyntheticEvent _ = False

js_isSyntheticUIEvent :: JSVal -> Bool
js_isSyntheticUIEvent _ = False

js_isSyntheticMouseEvent :: JSVal -> Bool
js_isSyntheticMouseEvent _ = False

js_isSyntheticKeyboardEvent :: JSVal -> Bool
js_isSyntheticKeyboardEvent _ = False

#endif
