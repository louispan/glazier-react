{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.DOM.Event.HashChange
  ( HashChangeEvent -- ^ constructor not exported
  , IHashChangeEvent(..)
  )
where

import qualified GHCJS.Types as J
import Glazier.DOM.Event
import Glazier.DOM.Event.HashChange.Internal
import qualified JavaScript.Extras as JE

-- | https://developer.mozilla.org/en-US/docs/Web/API/HashChangeEvent
-- https://reactjs.org/docs/events.html#HashChange-events
-- onScroll
class IEvent j => IHashChangeEvent j where
  oldURL :: j -> J.JSString
  oldURL = js_oldURL . JE.toJS

  newURL :: j -> J.JSString
  newURL  = js_newURL . JE.toJS

instance IHashChangeEvent HashChangeEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['oldURL']"
    js_oldURL :: J.JSVal -> J.JSString

foreign import javascript unsafe
    "$r = $1['newURL']"
    js_newURL :: J.JSVal -> J.JSString

#else

js_oldURL :: J.JSVal -> J.JSString
js_oldURL _ = mempty

js_newURL :: J.JSVal -> J.JSString
js_newURL _ = mempty

#endif
