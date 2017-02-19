{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Input where

import Control.Applicative as A
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.HashMap.Strict as M
import qualified Data.JSString as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E

data Model = Model
    { uid :: J.JSString
    , value :: J.JSString
    , fireChange :: J.Callback (J.JSVal -> IO ())
    , fireSubmit :: J.Callback (J.JSVal -> IO ())
    }

makeClassy_ ''Model

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.leaf (E.strval "input") (M.fromList
                    [ ("key", E.strval $ uid s)
                    , ("className", E.strval "new-todo")
                    , ("placeholder", E.strval "What needs to be done?")
                    , ("value", J.jsval $ value s)
                    , ("autoFocus", J.pToJSVal True)
                    , ("onChange", J.jsval $ fireChange s)
                    , ("onKeyDown", J.jsval $ fireSubmit s)
                    ])

data Action = ChangeAction J.JSString | SubmitAction

changeFirer :: J.JSVal -> MaybeT IO Action
changeFirer = R.eventHandlerM goStrict goLazy
    where
      goStrict :: J.JSVal -> MaybeT IO J.JSString
      goStrict evt = do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          -- target is the "input" DOM
          input <- lift $ pure . J.jsval . R.target . R.parseEvent $ evt'
          v <- lift $ E.getProperty "value" input
          MaybeT $ J.fromJSVal v

      goLazy :: J.JSString -> MaybeT IO Action
      goLazy = pure . ChangeAction

submitFirer :: J.JSVal -> MaybeT IO Action
submitFirer = R.eventHandlerM goStrict goLazy
    where
      goStrict :: J.JSVal -> MaybeT IO Int
      goStrict evt = do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          evt'' <- MaybeT $ pure $ R.parseKeyboardEvent evt'
          pure $ R.keyCode evt''

      goLazy :: Int -> MaybeT IO Action
      goLazy keyCode = if keyCode == 13 -- FIXME: ENTER_KEY
                       then pure SubmitAction
                       else A.empty

data Command = StateChangedCommand | SubmittedCommand J.JSString

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = do
    a <- ask
    case a of
        ChangeAction str -> do
            _value .= str
            pure $ D.singleton StateChangedCommand
        SubmitAction -> do
            -- trim the text
            _value %= J.strip
            v <- use _value
            pure (D.fromList [SubmittedCommand v, StateChangedCommand])
