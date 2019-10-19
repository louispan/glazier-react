{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- | https://developer.mozilla.org/en-US/docs/Web/API/EventTarget
module Glazier.DOM.EventTarget
    ( -- | constructor is not exported
      EventTarget
    , IEventTarget(..)
    )
    where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import Glazier.DOM.Event
import Glazier.DOM.EventTarget.Internal
import Glazier.React.Common
import qualified JavaScript.Extras as JE

-- | The object that dispatched the event.
-- https://developer.mozilla.org/en-US/docs/Web/API/EventTarget
class JE.ToJS j => IEventTarget j where
    addEventListener :: MonadIO m => j -> J.JSString -> Listener -> m ()
    addEventListener j n cb = liftIO $ js_addEventListener (JE.toJS j) n cb

    removeEventListener :: MonadIO m => j -> J.JSString -> Listener -> m ()
    removeEventListener j n cb = liftIO $ js_removeEventListener (JE.toJS j) n cb

    -- | throws, therefore @foreign import javascript safe@ so exceptions can be
    -- handled without killing haskell thread
    dispatchEvent :: MonadIO m => j -> NativeEvent -> m ()
    dispatchEvent j e = liftIO $ js_dispatchEvent (JE.toJS j) e

instance IEventTarget EventTarget

#ifdef __GHCJS__

foreign import javascript unsafe
    "if ($1 && $1['addEventListener']) { $1['addEventListener']($2, $3); }"
    js_addEventListener :: J.JSVal -> J.JSString -> Listener -> IO ()

foreign import javascript unsafe
    "if ($1 && $1['removeEventListener']) { $1['removeEventListener']($2, $3); }"
    js_removeEventListener :: J.JSVal -> J.JSString -> Listener -> IO ()

foreign import javascript safe -- since 'dispatchEvent throws
    "if ($1 && $1['dispatchEvent']) { $1['dispatchEvent']($2); }"
    js_dispatchEvent :: J.JSVal -> NativeEvent -> IO ()

#else

js_addEventListener :: J.JSVal -> J.JSString -> Listener -> IO ()
js_addEventListener _ _ _ = pure ()

js_removeEventListener :: J.JSVal -> J.JSString -> Listener -> IO ()
js_removeEventListener _ _ _ = pure ()

js_dispatchEvent :: J.JSVal -> NativeEvent -> IO ()
js_dispatchEvent _ _ = pure ()

#endif
