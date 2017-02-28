-- | Contains commons utilities when defining your own widget
module Glazier.React.Widget
  ( onRender
  , onRef
  , onUpdated
  , createGlazierElement
  , reactDomRender
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Trans.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Element as R
import qualified Glazier.React.Markup as R

-- | This is called synchronously by React to render the DOM.
-- This must not block!
onRender :: G.WindowT s (R.ReactMlT IO) () -> MVar s -> IO J.JSVal
onRender w s = do
    s' <- readMVar s
    xs <- view G._WindowT' (R.renderedWindow w) s'
    J.jsval <$> R.mkCombinedElements xs

onRef :: (J.JSVal -> act) -> J.JSVal -> MaybeT IO act
onRef f = pure . f

onUpdated :: (Int -> act) -> J.JSVal -> MaybeT IO act
onUpdated f =  R.eventHandlerM goStrict goLazy
  where
    -- goStrict :: J.JSVal -> MaybeT IO Int
    goStrict i = MaybeT $ J.fromJSVal i

    -- goLazy :: Int -> MaybeT IO act
    goLazy i = pure $ f i

-- | Creates the React rendering instructions using the Glazier react component
-- Glazier react component is specified in jsextras/Glazier.js
-- It is a simple React.PureComponent with render, ref, and updated callbacks.
createGlazierElement
      :: J.Callback (IO J.JSVal)
      -> J.Callback (J.JSVal -> IO ())
      -> J.Callback (J.JSVal -> IO ())
      -> IO J.JSVal
createGlazierElement = js_createGlazierElement

-- | Using a React Element (first arg) give React rendering control over a DOM element (second arg).
-- This should only be called for the topmost component.
reactDomRender :: J.JSVal -> J.JSVal -> IO ()
reactDomRender = js_reactDomRender

foreign import javascript unsafe
  "$r = React.createElement(Glazier, { render: $1, ref: $2, updated: $3 });"
  js_createGlazierElement
      :: J.Callback (IO J.JSVal)
      -> J.Callback (J.JSVal -> IO ())
      -> J.Callback (J.JSVal -> IO ())
      -> IO J.JSVal

foreign import javascript unsafe
  "ReactDOM.render($1, $2);"
  js_reactDomRender :: J.JSVal -> J.JSVal -> IO ()
