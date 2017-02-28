{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Dummy
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
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
import qualified Glazier.React.Widget as R

type FrameNum = Int

data Command
    -- | This should result in renderSeqNum being increased by one,
    -- and the model to be stored in the render TMVar
    -- and the following pseudo javascript to notify React of the state change:
    -- ref.setState({seqNum: renderSeqNum})
    -- NB. the seqNum are incremented by gadget and as a parameter to RenderCommand
    -- because incrementing the seqNum is an indication that a render request to React
    -- has been sent (not just requested).
    -- Incrementing the seqNum on the gadget side may result React not rendering a stale state.
    = RenderCommand
    | SubmitCommand J.JSString
    | SetSelectionCommand J.JSVal
                          Int
                          Int
                          J.JSString

data Action
    = RefAction J.JSVal
    | RenderedAction FrameNum
    | ChangeAction J.JSVal
                   Int
                   Int
                   J.JSString
                   J.JSString
    | SubmitAction

makeClassyPrisms ''Action

data Callbacks = Callbacks
    { onRender :: J.Callback (IO J.JSVal)
    , onRef :: J.Callback (J.JSVal -> IO ())
    , onUpdated :: J.Callback (J.JSVal -> IO ())
    , onChange :: J.Callback (J.JSVal -> IO ())
    , onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

instance CD.Disposing Callbacks

data Model = Model
    { ref :: J.JSVal
    -- | renderSeqNum is updated on react's componentDidUpdate callback with the new seqNum
    , frameNum :: FrameNum
    , deferredCommands :: M.Map FrameNum (D.DList Command)
    , uid :: J.JSString
    , value :: J.JSString
    , callbacks :: Callbacks
    }

makeClassy_ ''Model

mkCallbacks
    :: MVar Model
    -> ((J.JSVal -> MaybeT IO Action) -> IO (J.Callback (J.JSVal -> IO ())))
    -> IO Callbacks
mkCallbacks s f =
    Callbacks
    <$> (J.syncCallback' $ R.onRender window s)
    <*> (f $ R.onRef RefAction)
    <*> (f $ R.onUpdated RenderedAction)
    <*> (f onChange')
    <*> (f onKeyDown')

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (E.strval "input")
                    [ ("key", E.strval $ uid s)
                    , ("className", E.strval "new-todo")
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

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = do
    a <- ask
    case a of
        RefAction node -> do
            _ref .= node
            pure mempty

        ChangeAction n ss se sd str -> do
            _value .= str
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

        RenderedAction n -> do
            -- Run delayed commands that need to wait until a particular frame is rendered
            -- Eg focusing after other rendering changes
            -- All deferred commands with renderSeqNum lower than the rendered SeqNum is
            -- safe to run
            cmds <- use _deferredCommands
            let (cmds', leftoverCmds) = M.partitionWithKey (\k _ -> k < n) cmds
            _deferredCommands .= leftoverCmds
            pure . foldMap snd . M.toList $ cmds'
  where
    addCommand cmd Nothing = Just $ D.singleton cmd
    addCommand cmd (Just cmds') = Just (cmds' `D.snoc` cmd)
