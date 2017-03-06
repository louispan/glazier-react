{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.App.Run
    ( run
    ) where

import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens -- for contramap
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified Glazier.React.Command.Run as R
import qualified Glazier.React.Maker.Run as R.Maker
import qualified Pipes.Concurrent as PC
import Todo.App as TD.App
import qualified Todo.Input.Run as TD.Input
import qualified Todo.Todo.Run as TD.Todo

run :: (Action -> act) -> PC.Output act -> Command -> IO ()

run mapAction output (MakerCommand mks) = do
    act <- mapAction <$> iterM (R.Maker.run (contramap mapAction output)) mks
    void $ atomically $ PC.send output act

run mapAction output (SendActionsCommand acts) =
    void $ runMaybeT $ traverse_ (\act -> lift $ atomically $ PC.send output (mapAction act) >>= guard) acts

run _ _ (RenderCommand sm props j) = R.componentSetState sm props j

run _ _ (DisposeCommand x) = CD.dispose x

run mapAction output (InputCommand cmd) = TD.Input.run (mapAction . RequestNewTodoAction) output cmd

run mapAction output (TodosCommand (k, cmd)) = TD.Todo.run (mapAction (DestroyTodoAction k)) output cmd
