module Todo.Todo.Run
    ( run
    ) where

import Control.Concurrent.STM
import Control.Monad
import qualified GHCJS.Types as J
import qualified Glazier.React.Command.Run as R
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC
import Todo.Todo

run :: act -> PC.Output act -> Command -> IO ()
run destroyTodoAction output DestroyCommand = void $ atomically $ PC.send output destroyTodoAction

run _ _ (SetPropertyCommand prop j) = JE.setProperty prop j

run _ _ (RenderCommand sm props j) = R.componentSetState sm props j

run _ _ (FocusNodeCommand j) = js_focus j

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: J.JSVal -> IO ()
