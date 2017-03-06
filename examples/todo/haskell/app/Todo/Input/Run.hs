module Todo.Input.Run
    ( run
    ) where

import Control.Concurrent.STM
import Control.Monad
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC
import Todo.Input

run :: (J.JSString -> act) -> PC.Output act -> Command -> IO ()
run toSubmitAction output (SubmitCommand str) = void $ atomically $ PC.send output (toSubmitAction str)

run _ _ (SetPropertyCommand prop j) = JE.setProperty prop j
