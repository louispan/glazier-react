{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Todo.Todo
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Callbacks(..)
    , HasCallbacks(..)
    , mkCallbacks
    , Model(..)
    , HasModel(..)
    , CModel
    , MModel
    , mkMModel
    , window
    , gadget
    ) where

import Control.Applicative as A
import Control.Monad.Free.Church
import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Extras as E
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Nullable as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R

type FrameNum = Int

data Command
    -- Common widget commands
    -- | This should result in frameNum incremented by one;
    -- the model to be stored in the render TMVar;
    -- and the following pseudo javascript to notify React of the state change:
    -- ref.setState({frameNum: i})
    -- NB. the seqNum are incremented by the interpreter because incrementing the seqNum
    -- is an indication that a render request to React has been sent (not just requested).
    -- Incrementing the seqNum on the gadget side may result React not rendering a stale state.
    = RenderCommand
    -- widget specific commands
    | DestroyCommand
    | FocusNodeCommand J.JSVal
    | SetSelectionCommand J.JSVal
                          Int
                          Int
                          J.JSString

data Action
    -- Common widget actions
    = RefAction J.JSVal
    | RenderedAction FrameNum
    -- widget specific actions
    | InputRefAction J.JSVal
    | StartEditAction
    | ToggleCompleteAction
    | DestroyAction
    | CancelEditAction
    | SubmitAction
    | ChangeAction J.JSVal Int Int J.JSString J.JSString

makeClassyPrisms ''Action

data Callbacks = Callbacks
    -- common widget callbacks
    { _onRender :: J.Callback (IO J.JSVal)
    , _onRef :: J.Callback (J.JSVal -> IO ())
    , _onUpdated :: J.Callback (J.JSVal -> IO ())
    -- widget specific callbacks
    , _onInputRef :: J.Callback (J.JSVal -> IO ())
    , _fireToggleComplete :: J.Callback (J.JSVal -> IO ())
    , _fireStartEdit :: J.Callback (J.JSVal -> IO ())
    , _fireDestroy :: J.Callback (J.JSVal -> IO ())
    , _fireCancelEdit :: J.Callback (J.JSVal -> IO ())
    , _onChange :: J.Callback (J.JSVal -> IO ())
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving G.Generic

instance CD.Disposing Callbacks

makeClassy ''Callbacks

data Model = Model
    -- common widget model
    { _uid :: J.JSString
    , _ref :: J.JSVal -- ^ ref to react component object
    , _frameNum :: FrameNum -- ^ frameNum is incremented by RenderCommand interpreter
    , _deferredCommands :: M.Map FrameNum (D.DList Command)
    -- widget specifc model
    , _inputRef :: J.JSVal
    , _value :: J.JSString
    , _completed :: Bool
    , _editText :: Maybe J.JSString
    }

makeClassy ''Model

type CModel = (Callbacks, Model)

-- | This might be different per widget
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

instance CD.Disposing CModel where
    disposing s = CD.DisposeList
        [ s ^. callbacks . to CD.disposing
        , s ^. model . to CD.disposing
        ]

instance HasCallbacks CModel where
    callbacks = _1

instance HasModel CModel where
    model = _2

type MModel = (MVar CModel, CModel)

instance CD.Disposing MModel where
    disposing s = CD.DisposeList
        [ s ^. callbacks . to CD.disposing
        , s ^. model . to CD.disposing
        ]

instance HasCallbacks MModel where
    callbacks = _2 . callbacks

instance HasModel MModel where
    model = _2 . model

mkCallbacks :: MVar CModel -> F (R.Maker Action) Callbacks
mkCallbacks ms = Callbacks
    -- common widget callbacks
    <$> (R.mkRenderer ms render)
    <*> (R.mkHandler $ R.onRef RefAction)
    <*> (R.mkHandler $ R.onUpdated RenderedAction)
    -- widget specific callbacks
    <*> (R.mkHandler onInputRef')
    <*> (R.mkHandler fireToggleComplete')
    <*> (R.mkHandler fireStartEdit')
    <*> (R.mkHandler fireDestroy')
    <*> (R.mkHandler fireCancelEdit')
    <*> (R.mkHandler onChange')
    <*> (R.mkHandler onKeyDown')

mkMModel :: Model -> F (R.Maker Action) MModel
mkMModel s = R.mkMModel mkCallbacks $ \cbs -> (cbs, s)

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf R.shimComponent
        [ ("key",  s ^. model . uid . to J.jsval)
        , ("render", s ^. callbacks . onRender . to E.PureJSVal . to J.pToJSVal)
        , ("ref", s ^. callbacks . onRef . to E.PureJSVal . to J.pToJSVal)
        , ("updated", s ^. callbacks . onUpdated . to E.PureJSVal . to J.pToJSVal)
        ]

render :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.bh (E.strval "li") [("className", cns s)] $ do
        R.bh (E.strval "div") [ ("key", E.strval "view")
                              , ("className", E.strval "view")
                              ] $ do
            R.lf (E.strval "input") [ ("key", E.strval "toggle")
                                    , ("className", E.strval "toggle")
                                    , ("type", E.strval "checkbox")
                                    , ("checked", s ^. model . completed . to J.pToJSVal)
                                    , ("onChange", s ^. callbacks . fireToggleComplete . to J.jsval)
                                    ]
            R.bh (E.strval "label")  [ ("key", E.strval "label")
                                     , ("onDoubleClick", s ^. callbacks . fireStartEdit. to J.jsval)
                                     ] (s ^. model . value . to R.txt)
            R.lf (E.strval "button") [ ("key", E.strval "destroy")
                                     , ("className", E.strval "destroy")
                                     , ("onClick", s ^. callbacks . fireDestroy . to J.jsval)
                                     ]
        R.lf (E.strval "input") [ ("key", E.strval "todo-input")
                                , ("ref", s ^. callbacks . onInputRef . to J.jsval)
                                , ("className", E.strval "edit")
                                , ("value", s ^. model . editText . to (fromMaybe J.empty) . to J.jsval)
                                , ("checked", s ^. model . completed . to J.pToJSVal)
                                , ("onBlur", s ^. callbacks . fireCancelEdit . to J.jsval)
                                , ("onChange", s ^. callbacks . onChange . to J.jsval)
                                , ("onKeyDown", s ^. callbacks . onKeyDown . to J.jsval)
                                ]
  where
    cns s = E.classNames [("completed", s ^. model . completed), ("editing", s ^. model . editText . to isJust)]

onInputRef' :: Monad m => J.JSVal -> m Action
onInputRef' v = pure $ InputRefAction v

fireToggleComplete' :: Applicative m => J.JSVal -> m Action
fireToggleComplete' = const $ pure ToggleCompleteAction

fireStartEdit' :: Applicative m => J.JSVal -> m Action
fireStartEdit' = const $ pure StartEditAction

fireDestroy' :: Applicative m => J.JSVal -> m Action
fireDestroy' = const $ pure DestroyAction

fireCancelEdit' :: Applicative m => J.JSVal -> m Action
fireCancelEdit' = const $ pure CancelEditAction

onChange' :: J.JSVal -> MaybeT IO Action
onChange' = R.eventHandlerM goStrict goLazy
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

onKeyDown' :: J.JSVal -> MaybeT IO Action
onKeyDown' = R.eventHandlerM goStrict goLazy
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
        -- common widget actions

        RefAction node -> do
            ref .= node
            pure mempty

        RenderedAction n -> do
            -- Run delayed commands that need to wait until a particular frame is rendered
            -- Eg focusing after other rendering changes
            -- All deferred commands with renderSeqNum lower than the rendered SeqNum is
            -- safe to run
            cmds <- use deferredCommands
            let (cmds', leftoverCmds) = M.partitionWithKey (\k _ -> k < n) cmds
            deferredCommands .= leftoverCmds
            pure . foldMap snd . M.toList $ cmds'

        -- widget specific actions

        ToggleCompleteAction -> do
            completed %= not
            pure $ D.singleton RenderCommand

        InputRefAction v -> do
            inputRef .= v
            pure mempty

        StartEditAction -> do
            n <- use inputRef
            ret <- runMaybeT $ do
                n' <- MaybeT $ pure $ J.nullableToMaybe (J.Nullable n)
                value' <- use value
                editText .= Just value'
                -- Need to delay focusing until after the next render
                let cmd = FocusNodeCommand n'
                i <- use frameNum
                deferredCommands %= (M.alter (addCommand cmd) i)
                pure $ D.singleton RenderCommand

            maybe (pure mempty) pure ret

        DestroyAction -> pure $ D.singleton DestroyCommand

        CancelEditAction -> do
            editText .= Nothing
            pure $ D.singleton RenderCommand

        ChangeAction n ss se sd str -> do
            editText .= Just str
            -- Need to delay set cursor position until after the next render
            let cmd = SetSelectionCommand n ss se sd
            i <- use frameNum
            deferredCommands %= (M.alter (addCommand cmd) i)
            pure $ D.singleton RenderCommand

        SubmitAction -> do
            -- trim the text
            v <- (fmap J.strip) <$> use editText
            let v' = fromMaybe J.empty v
            value .= v'
            editText .= Nothing
            if J.null v'
                then pure $ D.singleton DestroyCommand
                else pure $ D.singleton RenderCommand
  where
    addCommand cmd Nothing = Just $ D.singleton cmd
    addCommand cmd (Just cmds') = Just (cmds' `D.snoc` cmd)
