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
import qualified Data.HashMap.Strict as M
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Nullable as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E

data Command
    = RenderRequiredCommand
    | DestroyCommand
    | DeferCommand Command
    | FocusNodeCommand J.JSVal

data Action
    = ToggleCompleteAction
    | StartEditAction
    | DestroyAction
    | CancelEditAction
    | SubmitAction
    | ChangeAction J.JSString
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
                                      , ("onChange", J.jsval $ s ^. _callbacks . to fireToggleComplete)
                                      ])
            R.branch "label"  (M.fromList
                                      [ ("key", E.strval "label")
                                      , ("onDoubleClick", J.jsval $ s ^. _callbacks . to fireStartEdit)
                                      ]) (R.txt $ value s)
            R.leaf (E.strval "button") (M.fromList
                                      [ ("key", E.strval "destroy")
                                      , ("className", E.strval "destroy")
                                      , ("onClick", J.jsval $ s ^. _callbacks . to fireDestroy)
                                      ])
        R.leaf (E.strval "input") (M.fromList
                                  [ ("key", E.strval "todo-input")
                                  , ("ref", J.jsval $ s ^. _callbacks . to fireSetEditNode)
                                  , ("className", E.strval "edit")
                                  , ("value", J.jsval $ editText s)
                                  , ("checked", J.pToJSVal $ completed s)
                                  , ("onBlur", J.jsval $ s ^. _callbacks . to fireCancelEdit)
                                  , ("onChange", J.jsval $ s ^. _callbacks . to fireChange)
                                  , ("onKeyDown", J.jsval $ s ^. _callbacks . to handleKeyDown)
                                  ])
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
        ChangeAction str -> do
            _editText .= str
            pure $ D.singleton RenderRequiredCommand
        SubmitAction -> do
            -- trim the text
            v <- J.strip <$> use _editText
            _value .= v
            _editText .= J.empty
            pure $ D.singleton RenderRequiredCommand
