{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Todo where

import Control.Applicative as A
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.HashMap.Strict as M
import qualified Data.JSString as J
import Data.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E

data Action
    = ToggleCompleteAction
    | StartEditAction
    | DestroyAction
    | CancelEditAction
    | SubmitAction
    | ChangeAction J.JSString

makeClassyPrisms ''Action

data Model = Model
    { uid :: J.JSString
    , value :: J.JSString
    , completed :: Bool
    , editText :: Maybe J.JSString
    , fireToggleComplete :: J.Callback (J.JSVal -> IO ())
    , fireStartEdit :: J.Callback (J.JSVal -> IO ())
    , fireDestroy :: J.Callback (J.JSVal -> IO ())
    , fireCancelEdit :: J.Callback (J.JSVal -> IO ())
    , fireChange :: J.Callback (J.JSVal -> IO ())
    , handleKeyDown :: J.Callback (J.JSVal -> IO ())
    }

makeClassy_ ''Model

classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = J.jsval . J.unwords . fmap fst . filter snd

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.branch "li" (M.singleton "className" (cns s)) $ do
        R.branch "div" (M.singleton "className" (E.strval "view")) $ do
            R.leaf (E.strval "input") (M.fromList
                                      [ ("className", E.strval "toggle")
                                      , ("type", E.strval "checkbox")
                                      , ("checked", J.pToJSVal $ completed s)
                                      , ("onChange", J.jsval $ fireToggleComplete s)
                                      ])
            R.branch "label" (M.singleton "onDoubleClick" (J.jsval $ fireStartEdit s)) (R.txt $ value s)
            R.leaf (E.strval "button") (M.fromList
                                      [ ("className", E.strval "destroy")
                                      , ("onClick", J.jsval $ fireDestroy s)
                                      ])
        R.leaf (E.strval "input") (M.fromList
                                  [ ("className", E.strval "edit")
                                  , ("value", J.pToJSVal (editText s))
                                  , ("checked", J.pToJSVal $ completed s)
                                  , ("onBlur", J.jsval $ fireCancelEdit s)
                                  , ("onChange", J.jsval $ fireChange s)
                                  , ("onKeyDown", J.jsval $ handleKeyDown s)
                                  ])
  where
    cns s = classNames [("completed", completed s), ("editing", isJust (editText s))]

toggleCompleteFirer :: Applicative m => J.JSVal -> m Action
toggleCompleteFirer = const $ pure ToggleCompleteAction

startEditFirer :: Applicative m => J.JSVal -> m Action
startEditFirer = const $ pure StartEditAction

destroyFirer :: Applicative m => J.JSVal -> m Action
destroyFirer = const $ pure DestroyAction

cancelEditFirer :: Applicative m => J.JSVal -> m Action
cancelEditFirer = const $ pure CancelEditAction

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

keyDownHandler :: J.JSVal -> MaybeT IO Action
keyDownHandler = R.eventHandlerM goStrict goLazy
    where
      goStrict :: J.JSVal -> MaybeT IO Int
      goStrict evt = do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          evt'' <- MaybeT $ pure $ R.parseKeyboardEvent evt'
          pure $ R.keyCode evt''

      goLazy :: Int -> MaybeT IO Action
      goLazy keyCode = case keyCode of
                           13 -> pure SubmitAction -- FIXME: ENTER_KEY
                           27 -> pure CancelEditAction -- FIXME: ESCAPE_KEY
                           _ -> A.empty

data Command = StateChangedCommand | SubmitCommand J.JSString | DestroyCommand

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = do
    a <- ask
    case a of
        ToggleCompleteAction -> do
            _completed %= not
            pure $ D.singleton StateChangedCommand
        StartEditAction -> pure mempty
        DestroyAction -> pure $ D.singleton DestroyCommand
        CancelEditAction -> pure mempty
        ChangeAction str -> do
            _value .= str
            pure $ D.singleton StateChangedCommand
        SubmitAction -> do
            -- trim the text
            _value %= J.strip
            v <- use _value
            pure (D.fromList [SubmitCommand v, StateChangedCommand])
