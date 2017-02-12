{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Control.Monad
-- import Data.Coerce
import Data.JSString (JSString, pack)
import Data.String
import qualified Data.Text.IO as T
import Data.Traversable
import GHCJS.Foreign.Callback
    ( Callback
    , OnBlocked(..)
    , syncCallback1
    , syncCallback1'
    )
-- import qualified Glazier.Pipes.Strict as GP
import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Types (IsJSVal, JSString, JSVal, jsval, nullRef)
import qualified Glazier.React.Event as R
import qualified Glazier.React.Window as R
import JavaScript.Array (JSArray, fromList)
import qualified Glazier as G
import Control.Monad.Trans.Reader

-- The user must remember to free with h$release(cb) if they no longer need the cb
main :: IO ()
main = do
    putStrLn "hello world"
    cb <- syncCallback1 ContinueAsync $ R.mkEventHandler
        (fmap (R.eventEventType . R.parseEvent) . R.castSyntheticEvent)
        (void . sequenceA . fmap T.putStrLn)
    -- It is not trivial to call arbitrary Haskell functions from Javascript
    -- A hacky way is to create a Callback and assign it to a global.
    -- In this example app, the webpage is always being rendered so the callback is not released.
    void $ js_globalAssignCallback "cb" cb
    extraRenderCb <- syncCallback1' (runReaderT (G.runWindow $ jsval <$> testWindow))
    void $ js_globalAssignCallback "renderExtra" extraRenderCb
    putStrLn "notifying"
    js_globalNotifyListeners "renderExtra" -- trigger a refresh
    putStrLn "finished setup"

foreign import javascript unsafe
  "h$glazier$react$todo[$1] = $2;"
  js_globalAssignCallback :: JSString -> Callback a -> IO ()

foreign import javascript unsafe
  "h$glazier$react$todo.notifyListeners($1);"
  js_globalNotifyListeners :: JSString -> IO ()

newtype JSFunction1 r = JSFunction1 JSVal
instance IsJSVal (JSFunction1 r)
instance PToJSVal (JSFunction1 r) where
    pToJSVal = jsval

-- foreign import javascript unsafe
--   "$r = function(props) { return React.createElement('p', null, props); };"
--   js_testRender :: JSFunction1 R.ReactElement

-- -- foreign import javascript unsafe
-- --   "$r = function(props) { return 'hello'; };"
-- --   js_testFunction :: JSFunction1 JSVal ReactElement

-- foreign import javascript unsafe
--   "$r = $1($2);"
--   js_callFunction1 :: JSFunction1 R.ReactElement -> JSVal -> R.ReactElement

-- testWindow :: G.Window IO JSVal [R.ReactElement]
-- testWindow = review G._Window $ \_ -> (: []) <$> R.createElement "h3" nullRef (fromList [jsval msg])
--   where
--     msg :: JSString
--     msg = "blah"

foreign import javascript unsafe
  "$r = React.createElement('div', null, $1);"
  js_testRender1 :: JSVal -> R.ReactElement

testWindow :: Applicative m => G.Window m JSVal R.ReactElement
testWindow = review G._Window $ \a -> pure $ js_testRender1 a


mkRender :: G.Window IO JSVal R.ReactElement -> (JSVal -> IO R.ReactElement)
mkRender (G.Window m) = runReaderT m
