{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.Input where

import Control.Lens
import qualified Data.HashMap.Strict as M
import qualified GHCJS.Types as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Marshal as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
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
    lift $ R.leaf (E.strval "input") (M.fromList
                    [ ("key", E.strval (s ^. _key))
                    , ("className", E.strval ("new-todo"))
                    , ("placeholder", E.strval ("What needs to be done?"))
                    , ("value", J.jsval (s ^. _value))
                    , ("onChange", J.jsval (s ^. _onChange))
                    , ("autoFocus", J.pToJSVal True)
                    ])

data InputAction = OnChange J.JSString

onChangeHandler :: J.JSVal -> MaybeT IO InputAction
onChangeHandler = R.eventHandlerM goStrict goLazy
    where
      goStrict :: J.JSVal -> MaybeT IO J.JSString
      goStrict evt = do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          -- target is the "input" DOM
          input <- lift $ pure . J.jsval . R.target . R.parseEvent $ evt'
          v <- lift $ E.getProperty "value" input
          MaybeT $ J.fromJSVal v

      goLazy :: J.JSString -> MaybeT IO InputAction
      goLazy = pure . OnChange

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
