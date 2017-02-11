{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( someFunc
    ) where

import Control.Monad
import Control.Concurrent.STM
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, syncCallback1)
import GHCJS.Types (jsval, JSVal)
import Glazier.React.Event
import qualified Data.Text.IO as T
import Data.Coerce
import Data.JSString (JSString, pack)
import Data.String

-- The user must remember to free with h$release(cb) if they no longer need the cb
someFunc :: IO ()
someFunc = do
    putStrLn "hello world"
    cb <- syncCallback1 ContinueAsync $ mkEventHandler (eventEventType . parseEvent . coerce) T.putStrLn
    -- It is not trivial to call arbitrary Haskell functions from Javascript
    -- A hacky way is to create a Callback (which needs to be released)
    -- and assign it to a global.
    void $ js_assignGlobalCallback "cb" cb
    void $ js_assignGlobalString "test" "wack"
    putStrLn "bye"

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
