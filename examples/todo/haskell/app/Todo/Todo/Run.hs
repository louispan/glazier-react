{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Todo.Run
    ( Env(..)
    , HasEnv(..)
    , run
    ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified GHCJS.Extras as E
import qualified GHCJS.Types as J
import qualified Glazier.React.Command.Run as R
import qualified Pipes.Concurrent as PC
import Todo.Todo

data Env act = Env
    { _output :: PC.Output act
    , _onDestroyTodo :: act
    }

makeClassy ''Env

run :: (HasEnv s a, MonadReader s io, MonadIO io) => Command -> io ()
run DestroyCommand = do
    output' <- view output
    act <- view onDestroyTodo
    liftIO $ void $ atomically $ PC.send output' act

run (SetPropertyCommand prop j) = liftIO $ E.setProperty prop j

run (RenderCommand sm props j) = liftIO $ R.componentSetState sm props j

run (FocusNodeCommand j) = liftIO $ js_focus j

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: J.JSVal -> IO ()
