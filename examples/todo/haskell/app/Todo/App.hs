{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Todo.App where

import qualified Data.HashMap.Strict as M
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import qualified Todo.Input as TD
import qualified Todo.Model as TD
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Lens

appWindow :: Monad m => G.WindowT TD.Model (R.ReactMlT m) ()
appWindow = do
    s <- ask
    lift $ R.branch "header" (M.fromList
                       [ ("className", R.strProp "header")
                       , ("key", R.strProp "todo")
                       ]) $ do
        R.branch "h1" (M.singleton "key" (R.strProp "heading")) (R.txt "todos")
        view G._WindowT todoInputWindow' s

todoInputWindow' :: Monad m => G.WindowT TD.Model (R.ReactMlT m) ()
todoInputWindow' = G.implant TD._todoInput TD.todoInputWindow
