{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.SyntheticEvent.Internal
    ( SyntheticEvent(..)
    ) where

import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import Glazier.React.DOM.Event

-- | Every event in React is a synthetic event, a cross-browser wrapper around the native event.
-- which reused from a pool (https://reactjs.org/docs/events.html).
-- So it is dangerous to keep a reference to a 'SyntheticEvent' since it may expire and contain
-- other things without you knowing.
-- All relevant data from the 'SyntheticEvent' must be consumed as soon you get one.
-- That is, 'SyntheticEvent' must only be used in the first strict part of 'Glazier.Reactor.React.mkHandler.
-- It is not an instance of NFData and so cannot be returned into the second lazy part of 'Glazier.Reactor.React.mkHandler'
newtype SyntheticEvent = SyntheticEvent J.JSVal
    deriving (G.Generic, Show, J.IsJSVal, J.PToJSVal, JE.ToJS, IsString)

instance JE.FromJS SyntheticEvent where
    validInstance = js_isSyntheticEvent
    fromJS a | js_isSyntheticEvent a = Just $ SyntheticEvent a
    fromJS _ = Nothing

instance IEvent SyntheticEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1 && $1['nativeEvent'] && $1['nativeEvent'] instanceof Event"
    js_isSyntheticEvent :: J.JSVal -> Bool

#else

js_isSyntheticEvent :: J.JSVal -> Bool
js_isSyntheticEvent _ = False

#endif
