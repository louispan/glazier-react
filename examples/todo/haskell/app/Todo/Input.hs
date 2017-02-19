{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.Input where

import Control.Applicative as A
import Control.Lens
import qualified Data.HashMap.Strict as M
import qualified GHCJS.Types as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Marshal as J
import qualified Data.JSString as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D

data InputModel = InputModel
    { key :: J.JSString
    , value :: J.JSString
    , onChange :: J.Callback (J.JSVal -> IO ())
    , onKeyDown :: J.Callback (J.JSVal -> IO ())
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
                    , ("autoFocus", J.pToJSVal True)
                    , ("onChange", J.jsval (s ^. _onChange))
                    , ("onKeyDown", J.jsval (s ^. _onKeyDown))
                    ])

data InputAction = InputChangedAction J.JSString | InputEnteredAction

inputOnChange :: J.JSVal -> MaybeT IO InputAction
inputOnChange = R.eventHandlerM goStrict goLazy
    where
      goStrict :: J.JSVal -> MaybeT IO J.JSString
      goStrict evt = do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          -- target is the "input" DOM
          input <- lift $ pure . J.jsval . R.target . R.parseEvent $ evt'
          v <- lift $ E.getProperty "value" input
          MaybeT $ J.fromJSVal v

      goLazy :: J.JSString -> MaybeT IO InputAction
      goLazy = pure . InputChangedAction

inputOnKeyDown :: J.JSVal -> MaybeT IO InputAction
inputOnKeyDown = R.eventHandlerM goStrict goLazy
    where
      goStrict :: J.JSVal -> MaybeT IO Int
      goStrict evt = do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          evt'' <- MaybeT $ pure $ R.parseKeyboardEvent evt'
          pure $ R.keyCode evt''

      goLazy :: Int -> MaybeT IO InputAction
      goLazy keyCode = if keyCode == 13 -- FIXME: ENTER_KEY
                       then pure InputEnteredAction
                       else A.empty

data InputCommand = InputStateChangedCommand | InputEnteredCommand J.JSString

inputGadget :: Monad m => G.GadgetT InputAction InputModel m (D.DList InputCommand)
inputGadget = do
    a <- ask
    case a of
        InputChangedAction str -> do
            _value .= str
            pure $ D.singleton InputStateChangedCommand
        InputEnteredAction -> do
            -- trim the text
            _value %= J.strip
            v <- use _value
            pure (D.fromList [InputEnteredCommand v, InputStateChangedCommand])
