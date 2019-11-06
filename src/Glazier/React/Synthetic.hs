{-# LANGUAGE CPP #-}

module Glazier.React.Synthetic
    ( -- | constructor is not exported
      SyntheticEvent
    , ISyntheticEvent(..)
    , SyntheticUIEvent
    , SyntheticMouseEvent
    , SyntheticKeyboardEvent
    )
    where

import Control.Monad.IO.Class
import Glazier.React.Synthetic.Internal
import JS.Data
import JS.DOM.Event
import JS.DOM.Event.Internal

-- | https://reactjs.org/docs/events.html
class IEvent j => ISyntheticEvent j where
    isPropagationStopped :: MonadIO m => j -> m Bool
    isPropagationStopped = liftIO . js_isPropagationStopped . toJS

    nativeEvent :: j -> Event
    nativeEvent = Event . js_nativeEvent . toJS

instance ISyntheticEvent SyntheticEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1.nativeEvent;"
    js_nativeEvent :: JSVal -> JSVal

foreign import javascript unsafe
    "$r = $1.isPropagationStopped;"
    js_isPropagationStopped :: JSVal -> IO Bool

#else

js_isPropagationStopped :: JSVal -> IO Bool
js_isPropagationStopped _ = pure False

js_nativeEvent :: JSVal -> JSVal
js_nativeEvent _ = nullRef

#endif
