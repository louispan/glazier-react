{-# LANGUAGE OverloadedStrings #-}

module Todo.Widget where

import Control.Applicative as A
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified GHCJS.Extras as E
import qualified GHCJS.Marshal as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Event as R

foreign import javascript unsafe
  "if ($1 && $1['value']) { $1['value'] = ''; }"
  js_resetValue :: J.JSVal -> IO ()

onInputKeyDown :: J.JSVal -> MaybeT IO (Maybe J.JSString, J.JSVal)
onInputKeyDown evt = do
        evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
        evt'' <- MaybeT $ pure $ R.parseKeyboardEvent evt'
        -- trget is the "input" DOM
        input <- lift $ pure . J.jsval . R.target . R.parseEvent $ evt'
        let k = R.keyCode evt''
        case k of
            -- FIXME: ESCAPE_KEY
            27 -> pure $ (Nothing, input)
            -- FIXME: ENTER_KEY
            13 -> do
                v <- MaybeT $ E.getProperty "value" input >>= J.fromJSVal
                pure $ (Just v, input)
            _ -> A.empty
