{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Todo.App where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as M
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Misc as PM
import qualified Todo.Input as TD.Input
import qualified Data.DList as D

-- | If state changed, then run the notifyListeners IO action
data Command = StateChangedCommand | TodoEnteredCommand J.JSString

data Action = InputAction TD.Input.Action

makeClassyPrisms ''Action

data Model = Model
    { todoInput :: TD.Input.Model
    }

makeClassy_ ''Model

window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.branch "header" (M.fromList
                       [ ("className", E.strval "header")
                       , ("key", E.strval "todo")
                       ]) $ do
        R.branch "h1" (M.singleton "key" (E.strval "heading")) (R.txt "todos")
        view G._WindowT inputWindow s

inputWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
inputWindow = G.implant _todoInput TD.Input.window

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = fmap go <$> G.implant _todoInput (G.dispatch _InputAction TD.Input.gadget)
  where
    go TD.Input.StateChangedCommand = StateChangedCommand
    go (TD.Input.EnteredCommand str) = TodoEnteredCommand str

inputOnChange :: J.JSVal -> MaybeT IO Action
inputOnChange v = (review _InputAction) <$> TD.Input.onChange v

inputOnKeyDown :: J.JSVal -> MaybeT IO Action
inputOnKeyDown v = (review _InputAction) <$> TD.Input.onKeyDown v

producer
    :: (MFunctor t, MonadState Model (t STM), MonadTrans t, MonadIO io)
    => PC.Input Action
    -> P.Producer' (D.DList Command) (t io) ()
producer input = hoist (hoist (liftIO . atomically)) (PM.rsProducer input (G.runGadgetT gadget))
