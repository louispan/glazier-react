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
import Control.Monad.Trans.Class
import qualified Data.HashMap.Strict as M
import Data.Monoid
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as R
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
                       [ ("className", R.strval "header")
                       , ("key", R.strval "todo")
                       ]) $ do
        R.branch "h1" (M.singleton "key" (R.strval "heading")) (R.txt "todos")
        view G._WindowT appInputWindow s

appInputWindow :: Monad m => G.WindowT AppModel (R.ReactMlT m) ()
appInputWindow = G.implant _todoInput TD.Input.inputWindow

appGadget :: Monad m => G.GadgetT AppAction AppModel m (First TD.Command)
appGadget = G.implant _todoInput (G.dispatch _AppInputAction TD.Input.inputGadget)

appInputOnChangedHandler :: J.JSVal -> IO AppAction
appInputOnChangedHandler = fmap (review _AppInputAction) <$> TD.Input.onChangeHandler

appProducer
    :: (MFunctor t, MonadState AppModel (t STM), MonadTrans t, MonadIO io)
    => PC.Input AppAction
    -> P.Producer' (First TD.Command) (t io) ()
appProducer input = hoist (hoist (liftIO . atomically)) (PM.rsProducer input (G.runGadgetT appGadget))
