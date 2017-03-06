{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Input.Run
    ( Env(..)
    , HasEnv(..)
    , run
    ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC
import Todo.Input

data Env act = Env
    { _output :: PC.Output act
    , _onSubmit :: J.JSString -> act
    }

makeClassy ''Env

run :: (HasEnv s act, MonadReader s io, MonadIO io) => Command -> io ()
run (SubmitCommand str) = do
    output' <- view output
    f <- view onSubmit
    liftIO $ void $ atomically $ PC.send output' (f str)

run (SetPropertyCommand prop j) = liftIO $ JE.setProperty prop j
