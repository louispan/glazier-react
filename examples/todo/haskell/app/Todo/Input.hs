{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.Input where

import Control.Lens
import qualified Data.HashMap.Strict as M
import qualified GHCJS.Types as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as R
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Todo.Command as TD
import Data.Monoid

data InputModel = InputModel
    { key :: J.JSString
    , value :: J.JSString
    , onChange :: J.Callback (J.JSVal -> IO ())
    }

makeClassy_ ''InputModel

-- | unsafe to enable lazy parsing. See mkEventHandler
foreign import javascript unsafe
  "console.log($1)"
  js_trace :: J.JSVal -> IO ()

inputWindow :: Monad m => G.WindowT InputModel (R.ReactMlT m) ()
inputWindow = do
    s <- ask
    lift $ R.leaf (R.strval "input") (M.fromList
                    [ ("key", R.strval (s ^. _key))
                    , ("className", R.strval ("new-todo"))
                    , ("placeholder", R.strval ("What needs to be done?"))
                    , ("value", J.jsval (s ^. _value))
                    , ("onChange", J.jsval (s ^. _onChange))
                    , ("autoFocus", J.pToJSVal True)
                    ])

data InputAction = OnChange J.JSString

onChangeHandler :: J.JSVal -> IO InputAction
onChangeHandler = R.eventHandlerIO goStrict goLazy
    where
      goStrict evt = void $ runMaybeT $ do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          r <- pure . J.jsval . R.target . R.parseEvent $ evt'
          lift $ js_trace r

      goLazy = const $ pure $ OnChange "world"

inputGadget :: Monad m => G.GadgetT InputAction InputModel m (First Command)
inputGadget = do
    a <- ask
    case a of
        OnChange str -> do
            _value .= str
            pure $ First $ Just TD.StateChangedCommand
        _ -> pure $ First Nothing


-- todoEventHandler'
-- wack :: Event -> InputAction
-- wack :: Event -> TodoInputAction
