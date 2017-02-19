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
import Data.Monoid
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as E
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Misc as PM
import qualified Todo.Input as TD.Input
import qualified Todo.Command as TD

data AppAction = AppInputAction TD.Input.InputAction

makeClassyPrisms ''AppAction

data AppModel = AppModel
    { todoInput :: TD.Input.InputModel
    }

makeClassy_ ''AppModel

appWindow :: Monad m => G.WindowT AppModel (R.ReactMlT m) ()
appWindow = do
    s <- ask
    lift $ R.branch "header" (M.fromList
                       [ ("className", E.strval "header")
                       , ("key", E.strval "todo")
                       ]) $ do
        R.branch "h1" (M.singleton "key" (E.strval "heading")) (R.txt "todos")
        view G._WindowT appInputWindow s

appInputWindow :: Monad m => G.WindowT AppModel (R.ReactMlT m) ()
appInputWindow = G.implant _todoInput TD.Input.inputWindow

appGadget :: Monad m => G.GadgetT AppAction AppModel m (First TD.Command)
appGadget = G.implant _todoInput (G.dispatch _AppInputAction TD.Input.inputGadget)

appInputOnChange :: J.JSVal -> MaybeT IO AppAction
appInputOnChange v = (review _AppInputAction) <$> TD.Input.inputOnChange v

appInputOnKeyDown :: J.JSVal -> MaybeT IO AppAction
appInputOnKeyDown v = (review _AppInputAction) <$> TD.Input.inputOnKeyDown v

appProducer
    :: (MFunctor t, MonadState AppModel (t STM), MonadTrans t, MonadIO io)
    => PC.Input AppAction
    -> P.Producer' (First TD.Command) (t io) ()
appProducer input = hoist (hoist (liftIO . atomically)) (PM.rsProducer input (G.runGadgetT appGadget))
