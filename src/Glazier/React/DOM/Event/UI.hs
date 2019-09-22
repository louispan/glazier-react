{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.DOM.Event.UI
  ( MouseEvent -- ^ constructor not exported
  , IUIEvent(..)
  )
where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import Glazier.React.DOM.Event.UI.Internal
import Glazier.React.DOM.EventTarget.Internal

class IEvent j => IUIEvent j where
  detail :: j -> Int

instance IUIEvent UIEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1['detail']"
    js_detail :: J.JSVal -> Int

#else

js_detail :: J.JSVal -> Int
js_detail _ = 0

#endif
