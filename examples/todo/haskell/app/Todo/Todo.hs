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
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Widget as R

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
    { onRender :: J.Callback (IO J.JSVal)
    , onRef :: J.Callback (J.JSVal -> IO ())
    , onUpdated :: J.Callback (J.JSVal -> IO ())
    -- widget specific callbacks
    , onInputRef :: J.Callback (J.JSVal -> IO ())
    , fireToggleComplete :: J.Callback (J.JSVal -> IO ())
    , fireStartEdit :: J.Callback (J.JSVal -> IO ())
    , fireDestroy :: J.Callback (J.JSVal -> IO ())
    , fireCancelEdit :: J.Callback (J.JSVal -> IO ())
    , onChange :: J.Callback (J.JSVal -> IO ())
    , onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving G.Generic

instance CD.Disposing Callbacks

data Model = Model
    -- common widget model
    { callbacks :: Callbacks
    , uid :: J.JSString
    , ref :: J.JSVal -- ^ ref to react component object
    , frameNum :: FrameNum -- ^ frameNum is incremented by RenderCommand interpreter
    , deferredCommands :: M.Map FrameNum (D.DList Command)
    -- widget specifc model
    , inputRef :: J.JSVal
    , value :: J.JSString
    , completed :: Bool
    , editText :: Maybe J.JSString
    }

makeClassy_ ''Model

mkCallbacks
    :: MVar Model
    -> ((J.JSVal -> MaybeT IO Action) -> IO (J.Callback (J.JSVal -> IO ())))
    -> IO Callbacks
mkCallbacks s f =
    Callbacks
    -- common widget callbacks
    <$> (J.syncCallback' $ R.onRender render s)
    <*> (f $ R.onRef RefAction)
    <*> (f $ R.onUpdated RenderedAction)
    -- widget specific callbacks
    <*> (f onInputRef')
    <*> (f fireToggleComplete')
    <*> (f fireStartEdit')
    <*> (f fireDestroy')
    <*> (f fireCancelEdit')
    <*> (f onChange')
    <*> (f onKeyDown')

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf R.shimComponent
        [ ("key",  J.jsval $ uid s)
        , ("render", J.pToJSVal . E.PureJSVal . onRender . callbacks $ s)
        , ("ref", J.pToJSVal . E.PureJSVal . onRef . callbacks $ s)
        , ("updated", J.pToJSVal . E.PureJSVal . onUpdated . callbacks $ s) ]

render :: Monad m => G.WindowT Model (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.bh (E.strval "li") [("className", cns s)] $ do
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
                                , ("ref", J.jsval $ s ^. _callbacks . to onInputRef)
                                , ("className", E.strval "edit")
                                , ("value", J.jsval $ fromMaybe J.empty $ editText s)
                                , ("checked", J.pToJSVal $ completed s)
                                , ("onBlur", J.jsval $ s ^. _callbacks . to fireCancelEdit)
                                , ("onChange", J.jsval $ s ^. _callbacks . to onChange)
                                , ("onKeyDown", J.jsval $ s ^. _callbacks . to onKeyDown)
                                ]
  where
    cns s = E.classNames [("completed", completed s), ("editing", isJust $ editText s)]

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
            _ref .= node
            pure mempty

        RenderedAction n -> do
            -- Run delayed commands that need to wait until a particular frame is rendered
            -- Eg focusing after other rendering changes
            -- All deferred commands with renderSeqNum lower than the rendered SeqNum is
            -- safe to run
            cmds <- use _deferredCommands
            let (cmds', leftoverCmds) = M.partitionWithKey (\k _ -> k < n) cmds
            _deferredCommands .= leftoverCmds
            pure . foldMap snd . M.toList $ cmds'

        -- widget specific actions

        ToggleCompleteAction -> do
            _completed %= not
            pure $ D.singleton RenderCommand

        InputRefAction v -> do
            _inputRef .= v
            pure mempty

        StartEditAction -> do
            n <- use _inputRef
            ret <- runMaybeT $ do
                n' <- MaybeT $ pure $ J.nullableToMaybe (J.Nullable n)
                value' <- use _value
                _editText .= Just value'
                -- Need to delay focusing until after the next render
                let cmd = FocusNodeCommand n'
                i <- use _frameNum
                _deferredCommands %= (M.alter (addCommand cmd) i)
                pure $ D.singleton RenderCommand

            maybe (pure mempty) pure ret

        DestroyAction -> pure $ D.singleton DestroyCommand

        CancelEditAction -> do
            _editText .= Nothing
            pure $ D.singleton RenderCommand

        ChangeAction n ss se sd str -> do
            _editText .= Just str
            -- Need to delay set cursor position until after the next render
            let cmd = SetSelectionCommand n ss se sd
            i <- use _frameNum
            _deferredCommands %= (M.alter (addCommand cmd) i)
            pure $ D.singleton RenderCommand

        SubmitAction -> do
            -- trim the text
            v <- (fmap J.strip) <$> use _editText
            let v' = fromMaybe J.empty v
            _value .= v'
            _editText .= Nothing
            if J.null v'
                then pure $ D.singleton DestroyCommand
                else pure $ D.singleton RenderCommand
  where
    addCommand cmd Nothing = Just $ D.singleton cmd
    addCommand cmd (Just cmds') = Just (cmds' `D.snoc` cmd)
