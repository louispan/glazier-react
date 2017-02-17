{-# LANGUAGE OverloadedStrings #-}

module Todo.Window where

import qualified Data.HashMap.Strict as M
import qualified Glazier as G
import qualified Glazier.React.Element as R
import qualified Glazier.React.Markup as R
import qualified Todo.Model as TD
import Control.Monad.Trans.Class
import Control.Monad.Reader
-- import qualified Data.Text as T
import GHCJS.Types (JSString, jsval)
import GHCJS.Marshal.Pure (PToJSVal(..))
import Control.Lens

todoWindow :: Monad m => G.WindowT TD.Model (R.ReactMlT m) ()
todoWindow = do
    s <- ask
    lift $ R.branch "header" (M.fromList
                       [ ("className", R.strProp "header")
                       , ("key", R.strProp "todo")
                       ]) $ do
        R.branch "h1" M.empty (R.txt "todos")
        view G._WindowT todoInputWindow' s
        R.branch "h2" M.empty (R.txt "todos2")

todoInputWindow :: Monad m => G.WindowT JSString (R.ReactMlT m) ()
todoInputWindow = do
    s <- ask
    lift $ R.leaf "input" (M.fromList
                    [ ("key", R.strProp "input")
                    , ("className", R.strProp "new-todo")
                    , ("placeholder", R.strProp "What needs to be done?")
                    , ("value", jsval s)
                    , ("autoFocus", pToJSVal True)
                    ])

todoInputWindow' :: Monad m => G.WindowT TD.Model (R.ReactMlT m) ()
todoInputWindow' = G.implant TD._todoInput todoInputWindow
