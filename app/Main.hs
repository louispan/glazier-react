{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Control.Monad
-- import Data.Coerce
import Data.JSString (JSString, pack)
import Data.String
import Data.Traversable
import qualified Data.Text.IO as T
import GHCJS.Foreign.Callback (Callback, OnBlocked(..), syncCallback1)
import GHCJS.Types (JSVal, jsval, nullRef)
import qualified Glazier as G
-- import qualified Glazier.Pipes.Strict as GP
import qualified Glazier.React.Event as R
import qualified Glazier.React.Window as R
import JavaScript.Array (JSArray)

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
    void $ js_assignGlobalCallback "cb" cb
    void $ js_assignGlobalString "test" "wack"
    putStrLn "finished setup"

foreign import javascript unsafe
  "$1()"
  runCallback :: Callback (IO ()) -> IO ()

foreign import javascript unsafe
  "h$glazier$react$todo[$1] = $2;"
  js_assignGlobal :: JSString -> JSVal -> IO ()

foreign import javascript unsafe
  "h$glazier$react$todo[$1] = $2;"
  js_assignGlobalString :: JSString -> JSString -> IO ()

foreign import javascript unsafe
  "h$glazier$react$todo[$1] = $2;"
  js_assignGlobalCallback :: JSString -> Callback a -> IO ()


-- staticWindow :: G.Window IO a [R.ReactElement]
-- staticWindow = review G._Window $ \_ -> (: []) <$> R.js_createElement "div" (R.ReactProps nullRef) (JSArray nullRef)
