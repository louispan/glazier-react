{-# LANGUAGE TemplateHaskell #-}
module Todo.Model where

import Control.Lens
import GHCJS.Types (JSString)

data Model = Model
    { todoInput :: JSString
    }

makeClassy_ ''Model
