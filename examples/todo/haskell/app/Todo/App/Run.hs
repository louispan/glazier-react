{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.App.Run
    ( runCommand
    ) where

import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Monad.Free.Church
import Control.Monad.State.Strict
import qualified Data.JSString as J
import qualified GHCJS.Extras as E
import qualified GHCJS.Types as J
import qualified Glazier.React.Command.Run as R
import qualified Glazier.React.Maker.Run as R
import qualified Pipes.Concurrent as PC
import qualified Todo.App as TD.App
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: J.JSVal -> IO ()

-- | Evaluate commands from gadgets here
runCommand
    :: PC.Output TD.App.Action
    -> TD.App.Command
    -> IO ()

runCommand output (TD.App.MakerCommand mks) = do
    act <- iterM (R.runMaker output) mks
    void $ atomically $ PC.send output act

runCommand output (TD.App.SendActionCommand act) = void $ atomically $ PC.send output act

runCommand _ (TD.App.RenderCommand sm props j) = R.componentSetState sm props j

runCommand _ (TD.App.DisposeCommand x) = CD.dispose x

runCommand output (TD.App.InputCommand (TD.Input.SubmitCommand str)) =
    void $ atomically $ PC.send output (TD.App.RequestNewTodoAction str)

runCommand _ (TD.App.InputCommand (TD.Input.SetPropertyCommand prop j)) =
    E.setProperty prop j

runCommand _ (TD.App.TodosCommand (_, TD.Todo.SetPropertyCommand prop j)) =
    E.setProperty prop j

runCommand _ (TD.App.TodosCommand (_, TD.Todo.RenderCommand sm props j)) =
    R.componentSetState sm props j

runCommand output (TD.App.TodosCommand (k, TD.Todo.DestroyCommand)) =
    void $ atomically $ PC.send output (TD.App.DestroyTodoAction k)

runCommand _ (TD.App.TodosCommand (_, TD.Todo.FocusNodeCommand node)) =
    js_focus node
