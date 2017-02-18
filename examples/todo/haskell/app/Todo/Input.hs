{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.Input where

import Control.Lens
import qualified Data.HashMap.Strict as M
import GHCJS.Types (JSString, jsval)
import GHCJS.Marshal.Pure (PToJSVal(..))
import qualified Glazier as G
import qualified Glazier.React.Markup as R
import Control.Monad.Reader

data InputModel = InputModel
    { key :: JSString
    , value :: JSString
    }

makeClassy_ ''InputModel

todoInputWindow :: Monad m => G.WindowT InputModel (R.ReactMlT m) ()
todoInputWindow = do
    s <- ask
    lift $ R.leaf "input" (M.fromList
                    [ ("key", R.strProp (s ^. _key))
                    , ("className", R.strProp "new-todo")
                    , ("placeholder", R.strProp "What needs to be done?")
                    , ("value", jsval (s ^. _value))
                    , ("autoFocus", pToJSVal True)
                    ])
