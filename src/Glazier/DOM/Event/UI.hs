{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.DOM.Event.UI
  ( -- | constructor not exported
    NativeUIEvent
    -- | constructor not exported
  , SyntheticUIEvent
  , IUIEvent(..)
  )
where

import qualified GHCJS.Types as J
import Glazier.DOM.Event
import Glazier.DOM.Event.UI.Internal
import Glazier.DOM.EventTarget.Window.Internal
import qualified JavaScript.Extras as JE

-- | https://developer.mozilla.org/en-US/docs/Web/API/UIEvent
-- https://reactjs.org/docs/events.html#ui-events
-- onScroll
class IEvent j => IUIEvent j where
  detail :: j -> Int
  detail = js_detail . JE.toJS

  view :: j -> Maybe Window
  view  = JE.fromJS . js_view . JE.toJS

instance IUIEvent NativeUIEvent
instance IUIEvent SyntheticUIEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['detail']"
    js_detail :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1['view']"
    js_view :: J.JSVal -> J.JSVal

#else

js_detail :: J.JSVal -> Int
js_detail _ = 0

js_view :: J.JSVal -> J.JSVal
js_view _ = J.nullRef

#endif
