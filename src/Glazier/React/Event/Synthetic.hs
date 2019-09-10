{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module based on React/Flux/PropertiesAndEvents.hs.
module Glazier.React.Event.Synthetic
  ( SyntheticEvent(..)
  , toSyntheticEvent
  )
where

import Control.DeepSeq
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import Glazier.React.EventTarget.Internal
import Glazier.React.NativeEvent.Internal
import Glazier.React.Notice.Internal

-- | Every `Notice` can be parsed to an `SyntheticEvent`.
-- 'SyntheticEvent' must only be used in the first part of 'handleEvent'.
data SyntheticEvent = SyntheticEvent
    { bubbles :: Bool
    , cancelable :: Bool
    , currentTarget :: EventTarget
    , defaultPrevented :: Bool
    , eventPhase :: Int
    , isTrusted :: Bool
    , nativeEvent :: NativeEvent
    , target :: EventTarget
    , timeStamp :: Int
    -- type is a reserved word, so prefix to eventType
    , eventType :: J.JSString
    }
    deriving (G.Generic)
instance NFData SyntheticEvent

-- | We can lie about this not being in IO because
-- within the strict part of 'handleEventM'
-- the Notice is effectively immutable.
toSyntheticEvent :: Notice -> SyntheticEvent
toSyntheticEvent (Notice evt) =
    SyntheticEvent
    { bubbles = unsafeGetProperty evt "bubbles"
    , cancelable = unsafeGetProperty evt "cancelable"
    , currentTarget = EventTarget $ unsafeGetProperty evt "currentTarget"
    , defaultPrevented = unsafeGetProperty evt "defaultPrevented"
    , eventPhase = unsafeGetProperty evt "eventPhase"
    , isTrusted = unsafeGetProperty evt "isTrusted"
    , nativeEvent = NativeEvent $ evt
    , target = EventTarget $ unsafeGetProperty evt "target"
    , timeStamp = unsafeGetProperty evt "timeStamp"
    , eventType = unsafeGetProperty evt "type"
    }

