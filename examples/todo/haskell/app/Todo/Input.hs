{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Input
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
import qualified GHC.Generics as G
import qualified GHCJS.Extras as E
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
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
    | SubmitCommand J.JSString
    | SetSelectionCommand J.JSVal
                          Int
                          Int
                          J.JSString

data Action
    -- Common widget actions
    = RefAction J.JSVal
    | RenderedAction FrameNum
    -- widget specific actions
    | ChangeAction J.JSVal
                   Int
                   Int
                   J.JSString
                   J.JSString
    | SubmitAction

makeClassyPrisms ''Action

data Callbacks = Callbacks
    -- common widget callbacks
    { onRender :: J.Callback (IO J.JSVal)
    , onRef :: J.Callback (J.JSVal -> IO ())
    , onUpdated :: J.Callback (J.JSVal -> IO ())
    -- widget specific callbacks
    , onChange :: J.Callback (J.JSVal -> IO ())
    , onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

instance CD.Disposing Callbacks

data Model = Model
    -- common widget model
    { callbacks :: Callbacks
    , uid :: J.JSString
    , ref :: J.JSVal -- ^ ref to react component object
    , frameNum :: FrameNum -- ^ frameNum is incremented by RenderCommand interpreter
    , deferredCommands :: M.Map FrameNum (D.DList Command)
    -- widget specifc model
    , value :: J.JSString
    }

makeClassy_ ''Model

mkCallbacks
    :: MVar Model
    -> ((J.JSVal -> MaybeT IO Action) -> IO (J.Callback (J.JSVal -> IO ())))
    -> IO Callbacks
mkCallbacks s f =
    Callbacks
    -- common widget callbacks
    <$> (J.syncCallback' $ R.onRender s render)
    <*> (f $ R.onRef RefAction)
    <*> (f $ R.onUpdated RenderedAction)
    -- widget specific callbacks
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

-- | This is used by the React render callback
render :: Monad m => G.WindowT Model (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.lf (E.strval "input")
                    [ ("className", E.strval "new-todo")
                    , ("placeholder", E.strval "What needs to be done?")
                    , ("value", J.jsval $ value s)
                    , ("autoFocus", J.pToJSVal True)
                    , ("onChange", J.jsval . onChange $ callbacks s)
                    , ("onKeyDown", J.jsval . onKeyDown $ callbacks s)
                    ]

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
    goLazy keyCode = if keyCode == 13 -- FIXME: ENTER_KEY
                     then pure SubmitAction
                     else A.empty

-- | State update logic.
-- The best practice is to leave this in general Monad m (eg, not MonadIO).
-- This allows gadget to use STM as the base monad which allows for combining concurrently
-- with other stateful STM effects and still maintain a single source of truth.
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

        ChangeAction n ss se sd str -> do
            _value .= str
            -- Need to delay set cursor position until after the next render
            let cmd = SetSelectionCommand n ss se sd
            i <- use _frameNum
            _deferredCommands %= (M.alter (addCommand cmd) i)
            pure $ D.singleton RenderCommand

        SubmitAction -> do
            v <- J.strip <$> use _value
            _value .= J.empty
            if J.null v
                then pure mempty
                else pure (D.fromList [SubmitCommand v, RenderCommand])

  where
    addCommand cmd Nothing = Just $ D.singleton cmd
    addCommand cmd (Just cmds') = Just (cmds' `D.snoc` cmd)
