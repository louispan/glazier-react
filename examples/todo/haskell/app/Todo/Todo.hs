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
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Nullable as J
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
    | SetEditNodeAction J.JSVal

makeClassyPrisms ''Action

data Model = Model
    { uid :: J.JSString
    , value :: J.JSString
    , completed :: Bool
    , editText :: J.JSString
    , editNode :: J.JSVal
    , fireSetEditNode :: J.Callback (J.JSVal -> IO ())
    , fireToggleComplete :: J.Callback (J.JSVal -> IO ())
    , fireStartEdit :: J.Callback (J.JSVal -> IO ())
    , fireDestroy :: J.Callback (J.JSVal -> IO ())
    , fireCancelEdit :: J.Callback (J.JSVal -> IO ())
    , fireChange :: J.Callback (J.JSVal -> IO ())
    , handleKeyDown :: J.Callback (J.JSVal -> IO ())
    }

makeClassy_ ''Model

getGarbage :: Model -> [E.Garbage]
getGarbage s =
    [ E.scrap $ fireSetEditNode s
    , E.scrap $ fireToggleComplete s
    , E.scrap $ fireStartEdit s
    , E.scrap $ fireDestroy s
    , E.scrap $ fireCancelEdit s
    , E.scrap $ fireChange s
    , E.scrap $ handleKeyDown s
    ]

classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = J.jsval . J.unwords . fmap fst . filter snd

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.branch "li" (M.fromList [ ("key", J.jsval $ uid s)
                                     , ("className", cns s)
                                     ]) $ do
        R.branch "div" (M.fromList [ ("key", E.strval "view")
                                   , ("className", E.strval "view")
                                   ]) $ do
            R.leaf (E.strval "input") (M.fromList
                                      [ ("key", E.strval "toggle")
                                      , ("className", E.strval "toggle")
                                      , ("type", E.strval "checkbox")
                                      , ("checked", J.pToJSVal $ completed s)
                                      , ("onChange", J.jsval $ fireToggleComplete s)
                                      ])
            R.branch "label"  (M.fromList
                                      [ ("key", E.strval "label")
                                      , ("onDoubleClick", J.jsval $ fireStartEdit s)
                                      ]) (R.txt $ value s)
            R.leaf (E.strval "button") (M.fromList
                                      [ ("key", E.strval "destroy")
                                      , ("className", E.strval "destroy")
                                      , ("onClick", J.jsval $ fireDestroy s)
                                      ])
        R.leaf (E.strval "input") (M.fromList
                                  [ ("key", E.strval "todo-input")
                                  , ("ref", J.jsval $ fireSetEditNode s)
                                  , ("className", E.strval "edit")
                                  , ("value", J.jsval $ editText s)
                                  , ("checked", J.pToJSVal $ completed s)
                                  , ("onBlur", J.jsval $ fireCancelEdit s)
                                  , ("onChange", J.jsval $ fireChange s)
                                  , ("onKeyDown", J.jsval $ handleKeyDown s)
                                  ])
  where
    cns s = classNames [("completed", completed s), ("editing", not . J.null $ editText s)]

foreign import javascript unsafe
  "$r = function(huh) { console.log(huh); };"
  js_wack :: J.JSVal

setEditNodeFirer :: Monad m => J.JSVal -> m Action
setEditNodeFirer v = pure $ SetEditNodeAction v

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

data Command = StateChangedCommand | DestroyCommand | StartEditCommand J.JSVal

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = do
    a <- ask
    case a of
        ToggleCompleteAction -> do
            _completed %= not
            pure $ D.singleton StateChangedCommand
        SetEditNodeAction v -> do
            _editNode .= v
            pure mempty
        StartEditAction -> do
            n <- use _editNode
            ret <- runMaybeT $ do
                n' <- MaybeT $ pure $ J.nullableToMaybe (J.Nullable n)
                value' <- use _value
                _editText .= value'
                pure $ D.fromList [StartEditCommand n', StateChangedCommand]
            maybe (pure mempty) pure ret
        DestroyAction -> pure $ D.singleton DestroyCommand
        CancelEditAction -> pure mempty
        ChangeAction str -> do
            _value .= str
            pure $ D.singleton StateChangedCommand
        SubmitAction -> do
            -- trim the text
            _value %= J.strip
            pure $ D.singleton StateChangedCommand
