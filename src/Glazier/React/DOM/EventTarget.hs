{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- | https://developer.mozilla.org/en-US/docs/Web/API/EventTarget
module Glazier.React.DOM.EventTarget
    ( EventTarget -- ^ constructor is not exported
    , IEventTarget(..)
    -- , listenEventTarget
    )
    where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import Glazier.React.Common
import Glazier.React.DOM.Event
import Glazier.React.DOM.EventTarget.Internal
import qualified JavaScript.Extras as JE
-- import Glazier.React.Reactor
-- import Glazier.React.Widget


-- -- | Add a listener with an event target, and automatically removes it on widget destruction
-- -- This only does something during initialization
-- listenEventTarget ::
--     (NFData a, HasCallStack, MonadWidget' c m, IEventTarget j)
--     => j -> J.JSString -> (J.JSVal -> MaybeT IO a) -> (a -> m ()) -> m ()
-- listenEventTarget j n goStrict goLazy =
--     onConstruction $ do
--         hdl <- mkHandler goStrict goLazy
--         cb <- mkListener hdl
--         liftIO $ addEventListener j' n cb
--         onDestruction $ liftIO $ removeEventListener j' n cb
--   where
--     j' = toJS j

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
