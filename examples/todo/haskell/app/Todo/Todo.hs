{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Todo
 ( Command(..)
 , Action(..)
 , AsAction(..)
 , Callbacks(..)
 , Model(..)
 , HasModel(..)
 , mkCallbacks
 , window
 , gadget
 ) where

import Control.Applicative as A
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Extras as E
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Nullable as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R

data Command
    = RenderRequiredCommand
    | DestroyCommand
    | DeferCommand Command
    | FocusNodeCommand J.JSVal
    | SetSelectionCommand J.JSVal
                          Int
                          Int
                          J.JSString

data Action
    = StartEditAction
    | ToggleCompleteAction
    | DestroyAction
    | CancelEditAction
    | SubmitAction
    | ChangeAction J.JSVal Int Int J.JSString J.JSString
    | SetEditNodeAction J.JSVal

makeClassyPrisms ''Action

data Callbacks = Callbacks
    { fireSetEditNode :: J.Callback (J.JSVal -> IO ())
    , fireToggleComplete :: J.Callback (J.JSVal -> IO ())
    , fireStartEdit :: J.Callback (J.JSVal -> IO ())
    , fireDestroy :: J.Callback (J.JSVal -> IO ())
    , fireCancelEdit :: J.Callback (J.JSVal -> IO ())
    , fireChange :: J.Callback (J.JSVal -> IO ())
    , handleKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving G.Generic

instance CD.Disposing Callbacks

data Model = Model
    { uid :: J.JSString
    , value :: J.JSString
    , completed :: Bool
    , editText :: J.JSString
    , editNode :: J.JSVal
    , callbacks :: Callbacks
    }

makeClassy_ ''Model

mkCallbacks :: ((J.JSVal -> MaybeT IO Action) -> IO (J.Callback (J.JSVal -> IO ()))) -> IO Callbacks
mkCallbacks f =
    Callbacks
    <$> (f setEditNodeFirer)
    <*> (f toggleCompleteFirer)
    <*> (f startEditFirer)
    <*> (f destroyFirer)
    <*> (f cancelEditFirer)
    <*> (f changeFirer)
    <*> (f keyDownHandler)

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.bh (E.strval "li") [ ("key", J.jsval $ uid s)
                                , ("className", cns s)
                                ] $ do
        R.bh (E.strval "div") [ ("key", E.strval "view")
                              , ("className", E.strval "view")
                              ] $ do
            R.lf (E.strval "input") [ ("key", E.strval "toggle")
                                    , ("className", E.strval "toggle")
                                    , ("type", E.strval "checkbox")
                                    , ("checked", J.pToJSVal $ completed s)
                                    , ("onChange", J.jsval $ s ^. _callbacks . to fireToggleComplete)
                                    ]
            R.bh (E.strval "label")  [ ("key", E.strval "label")
                                     , ("onDoubleClick", J.jsval $ s ^. _callbacks . to fireStartEdit)
                                     ] (R.txt $ value s)
            R.lf (E.strval "button") [ ("key", E.strval "destroy")
                                     , ("className", E.strval "destroy")
                                     , ("onClick", J.jsval $ s ^. _callbacks . to fireDestroy)
                                     ]
        R.lf (E.strval "input") [ ("key", E.strval "todo-input")
                                , ("ref", J.jsval $ s ^. _callbacks . to fireSetEditNode)
                                , ("className", E.strval "edit")
                                , ("value", J.jsval $ editText s)
                                , ("checked", J.pToJSVal $ completed s)
                                , ("onBlur", J.jsval $ s ^. _callbacks . to fireCancelEdit)
                                , ("onChange", J.jsval $ s ^. _callbacks . to fireChange)
                                , ("onKeyDown", J.jsval $ s ^. _callbacks . to handleKeyDown)
                                ]
  where
    cns s = E.classNames [("completed", completed s), ("editing", not . J.null $ editText s)]

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

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = do
    a <- ask
    case a of
        ToggleCompleteAction -> do
            _completed %= not
            pure $ D.singleton RenderRequiredCommand

        SetEditNodeAction v -> do
            _editNode .= v
            pure mempty

        StartEditAction -> do
            n <- use _editNode
            ret <- runMaybeT $ do
                n' <- MaybeT $ pure $ J.nullableToMaybe (J.Nullable n)
                value' <- use _value
                _editText .= value'
                -- Need to delay focusing until after the next render
                pure $ D.fromList [ DeferCommand (FocusNodeCommand n')
                                  , RenderRequiredCommand
                                  ]
            maybe (pure mempty) pure ret

        DestroyAction -> pure $ D.singleton DestroyCommand

        CancelEditAction -> do
            _editText .= J.empty
            pure $ D.singleton RenderRequiredCommand

        ChangeAction n ss se sd str -> do
            _editText .= str
            pure $ D.fromList
                [ DeferCommand (SetSelectionCommand n ss se sd)
                , RenderRequiredCommand
                ]

        SubmitAction -> do
            -- trim the text
            v <- J.strip <$> use _editText
            _value .= v
            _editText .= J.empty
            pure $ D.singleton RenderRequiredCommand
