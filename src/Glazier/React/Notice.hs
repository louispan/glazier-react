{-# LANGUAGE CPP #-}

module Glazier.React.DOM.Event.Notice
    ( Notice  -- constructor is not exported
    , preventDefault
    , isDefaultPrevented
    , stopPropagation
    , isPropagationStopped
    )
    where

import Control.Monad.IO.Class
import Glazier.React.DOM.Event.Notice.Internal

preventDefault :: MonadIO m => Notice -> m ()
preventDefault = js_preventDefault

isDefaultPrevented :: Notice -> Bool
isDefaultPrevented = js_isDefaultPrevented

stopPropagation :: MonadIO m => Notice -> m ()
stopPropagation = js_stopPropagation

isPropagationStopped :: Notice -> Bool
isPropagationStopped = js_isPropagationStopped

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1['preventDefault']()"
    js_preventDefault :: MonadIO m => Notice -> m ()

foreign import javascript unsafe
    "$1['isDefaultPrevented']()"
    js_isDefaultPrevented :: Notice -> Bool

foreign import javascript unsafe
    "$1['stopPropagation']()"
    js_stopPropagation :: MonadIO m => Notice -> m ()

foreign import javascript unsafe
    "$1['isPropagationStopped']()"
    js_isPropagationStopped :: Notice -> Bool

#else

js_preventDefault :: MonadIO m => Notice -> m ()
js_preventDefault _ = pure ()

js_isDefaultPrevented :: Notice -> Bool
js_isDefaultPrevented _ = False

js_stopPropagation :: MonadIO m => Notice -> m ()
js_stopPropagation _ = pure ()

js_isPropagationStopped :: Notice -> Bool
js_isPropagationStopped _ = False

#endif
