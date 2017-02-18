{-# LANGUAGE TemplateHaskell #-}

module Todo.Model
    ( HasModel(..)
    , Model(..)
    , module TD
    ) where

import Control.Lens
import Todo.Input as TD

data Model = Model
    { todoInput :: TD.InputModel
    }

makeClassy_ ''Model
