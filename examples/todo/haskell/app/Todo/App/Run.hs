{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.App.Run
    ( Env(..)
    , HasEnv(..)
    , run
    ) where

import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified Glazier.React.Command.Run as R
import qualified Glazier.React.Maker.Run as R.Maker
import qualified Pipes.Concurrent as PC
import Todo.App as TD.App
import qualified Todo.Input.Run as TD.Input
import qualified Todo.Todo.Run as TD.Todo

data Env act = Env
    { _output :: PC.Output act
    , _mapAction :: Action -> act
    }

makeClassy ''Env

run :: (HasEnv s act, MonadReader s io, MonadIO io) => Command -> io ()

run (MakerCommand mks) = do
    output' <- view output
    f <- view mapAction
    act <- liftIO $ f <$> iterM (R.Maker.run (contramap f output')) mks
    liftIO $ void $ atomically $ PC.send output' act

run (SendActionsCommand acts) = do
    output' <- view output
    f <- view mapAction
    liftIO $ void $ runMaybeT $ traverse_ (\act -> lift $ atomically $ PC.send output' (f act) >>= guard) acts

run (RenderCommand sm props j) = liftIO $ R.componentSetState sm props j

run (DisposeCommand x) = liftIO $ CD.dispose x

run (InputCommand cmd) = do
    output' <- view output
    f <- view mapAction
    runReaderT
        (TD.Input.run cmd)
        (TD.Input.Env output' (f . RequestNewTodoAction))

run (TodosCommand (k, cmd)) = do
    output' <- view output
    f <- view mapAction
    runReaderT
        (TD.Todo.run cmd)
        (TD.Todo.Env output' (f (DestroyTodoAction k)))
