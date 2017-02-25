{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Input where

import Control.Applicative as A
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E

data Command
    = RenderRequiredCommand
    | DeferCommand Command
    | SubmitCommand J.JSString
    | SetSelectionCommand J.JSVal
                          Int
                          Int
                          J.JSString

data Callbacks = Callbacks
    { fireChange :: J.Callback (J.JSVal -> IO ())
    , fireSubmit :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

instance CD.Disposing Callbacks

data Model = Model
    { uid :: J.JSString
    , value :: J.JSString
    , callbacks :: Callbacks
    }

makeClassy_ ''Model

data Action = ChangeAction J.JSVal Int Int J.JSString J.JSString | SubmitAction

mkCallbacks :: ((J.JSVal -> MaybeT IO Action) -> IO (J.Callback (J.JSVal -> IO ()))) -> IO Callbacks
mkCallbacks f =
    Callbacks
    <$> (f changeFirer)
    <*> (f submitFirer)

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (E.strval "input")
                    [ ("key", E.strval $ uid s)
                    , ("className", E.strval "new-todo")
                    , ("placeholder", E.strval "What needs to be done?")
                    , ("value", J.jsval $ value s)
                    , ("autoFocus", J.pToJSVal True)
                    , ("onChange", J.jsval $ fireChange $ callbacks s)
                    , ("onKeyDown", J.jsval $ fireSubmit $ callbacks s)
                    ]

changeFirer :: J.JSVal -> MaybeT IO Action
changeFirer = R.eventHandlerM goStrict goLazy
    where
      goStrict :: J.JSVal -> MaybeT IO (J.JSVal, Int, Int, J.JSString, J.JSString)
      goStrict evt = do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          -- target is the "input" DOM
          input <- lift $ pure . J.jsval . R.target . R.parseEvent $ evt'
          ss <- MaybeT $ E.getProperty "selectionStart" input >>= J.fromJSVal
          se <- MaybeT $ E.getProperty "selectionEnd" input >>= J.fromJSVal
          sd <- MaybeT $ E.getProperty "selectionDirection" input >>= J.fromJSVal
          v <- MaybeT $ E.getProperty "value" input >>= J.fromJSVal
          pure $ (input, ss, se, sd, v)

      goLazy :: (J.JSVal, Int, Int, J.JSString, J.JSString) -> MaybeT IO Action
      goLazy (n, ss, se, sd, v) = pure $ ChangeAction n ss se sd v

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

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = do
    a <- ask
    case a of
        ChangeAction n ss se sd str -> do
            _value .= str
            pure $ D.fromList
                [ DeferCommand (SetSelectionCommand n ss se sd)
                , RenderRequiredCommand
                ]
        SubmitAction -> do
            v <- J.strip <$> use _value
            _value .= J.empty
            if J.null v
                then pure mempty
                else pure (D.fromList [SubmitCommand v, RenderRequiredCommand])
