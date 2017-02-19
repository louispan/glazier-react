{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.App
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Model(..)
    , HasModel(..)
    , window
    , gadget
    , toggleAllHandler
    , inputChangeHandler
    , inputKeyDownHandler
    , producer
    ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.HashMap.Strict as M
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Misc as PM
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo
import Data.Semigroup

-- | If state changed, then run the notifyListeners IO action
data Command = StateChangedCommand | TodoEnteredCommand J.JSString

data Action = ToggleAllAction | InputAction TD.Input.Action

makeClassyPrisms ''Action

data Model = Model
    { todoInput :: TD.Input.Model
    , todos :: [TD.Todo.Model]
    , onToggleAllHandler :: J.Callback (J.JSVal -> IO ())
    }

makeClassy_ ''Model

toggleAllHandler :: Applicative m => J.JSVal -> m Action
toggleAllHandler = const $ pure ToggleAllAction

inputChangeHandler :: J.JSVal -> MaybeT IO Action
inputChangeHandler v = (review _InputAction) <$> TD.Input.changeHandler v

inputKeyDownHandler :: J.JSVal -> MaybeT IO Action
inputKeyDownHandler v = (review _InputAction) <$> TD.Input.keyDownHandler v

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.branch "header" (M.fromList
                       [ ("className", E.strval "header")
                       , ("key", E.strval "todo")
                       ]) $ do
        R.branch "h1" (M.singleton "key" (E.strval "heading")) (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT mainWindow s

mainWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
mainWindow = do
    todos' <- view _todos
    if (null todos')
        then pure ()
        else do
        s <- ask
        lift $ R.branch "section" (M.singleton "className" (E.strval "main")) $
            -- This is the complete all checkbox
            R.leaf (E.strval "input") (M.fromList
                        [ ("className", E.strval "toggle-all")
                        , ("type", E.strval "checkbox")
                        , ("checked", J.pToJSVal (not (hasActiveTodos todos')))
                        , ("onChange", J.jsval (s ^. _onToggleAllHandler))
                        ])

hasActiveTodos :: [TD.Todo.Model] -> Bool
hasActiveTodos = null . filter (not . TD.Todo.completed)

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = appGadget <> inputGadget

appGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
appGadget = do
    a <- ask
    case a of
        ToggleAllAction -> do
            _todos %= go
            pure $ D.singleton StateChangedCommand
        _ -> pure mempty -- ^ delegate to other gadgets
  where
    go :: [TD.Todo.Model] -> [TD.Todo.Model]
    go xs = fmap (go' (hasActiveTodos xs)) xs
    go' :: Bool -> TD.Todo.Model -> TD.Todo.Model
    go' b s = s & TD.Todo._completed .~ b


inputWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
inputWindow = G.implant _todoInput TD.Input.window

inputGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
inputGadget = fmap go <$> G.implant _todoInput (G.dispatch _InputAction TD.Input.gadget)
  where
    go TD.Input.StateChangedCommand = StateChangedCommand
    go (TD.Input.EnteredCommand str) = TodoEnteredCommand str

producer
    :: (MFunctor t, MonadState Model (t STM), MonadTrans t, MonadIO io)
    => PC.Input Action
    -> P.Producer' (D.DList Command) (t io) ()
producer input = hoist (hoist (liftIO . atomically)) (PM.rsProducer input (G.runGadgetT gadget))
