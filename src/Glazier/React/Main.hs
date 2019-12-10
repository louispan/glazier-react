{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Main
    ( LogConfig(..)
    , simpleWidgetMain
    , AppCmd'
    , execApp
    , module Glazier.React.Exec
    ) where

import Control.Concurrent
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Cont
import Data.Foldable
import Data.IORef
import Data.Tagged.Extras
import Data.Typeable
import GHCJS.Foreign.Export
import Glazier.React.Exec
import qualified JS.DOM as DOM

-----------------------------------------------

-- | Add a newtype wrapper to allow recursive definition
type instance AppCmds "Simple" c = [[c], IO c, LogLine JSString, Reactant c]
type AppCmd' = AppCmd "Simple"

execApp :: (
    c ~ AppCmd "Simple"
    , Cmd' [] c
    , Cmd' IO c
    , Cmd (LogLine JSString) c
    , Cmd' Reactant c
    , MonadUnliftIO m
    , AskLogConfigRef m
    , AskNextReactIdRef m
    , AskDirtyPlans m
    ) => c -> m ()
execApp = fixVerifyExec (unAppCmd @"Simple") maybeExecApp
  where
    maybeExecApp executor =
         maybeExec (traverse_ @[] executor) -- import Data.Foldable
        `orExec` maybeExec ((>>= executor) . liftIO)
        `orExec` maybeExec execLogLineJS
        `orExec` maybeExec (execReactant executor)

-- | An example of starting an app using the glazier-react framework
-- A different @Obj o@ will be create everytime this function is used
simpleWidgetMain ::
    ( Typeable s
    )
    => LogConfig
    -> Int
    -> DOM.Element
    -> Widget s AppCmd' ()
    -> LogName
    -> s
    -> IO (Maybe (Export (Obj s)))
simpleWidgetMain logCfg rerenderDelayMicroseconds root wid logname s = do
    -- Create the Environ required by the executor
    logConfigRef <- newIORef logCfg
    nextReactIdRef <- Tagged @"NextReactId" <$> newIORef @Int 0
    dirtyPlansRef <- Tagged @"DirtyPlans" <$> newIORef @DirtyPlans mempty
    rb <- mkReactBatch

    void $ forkIO $ forever $ do
        threadDelay rerenderDelayMicroseconds
        void . (`runReaderT` rb)
            . (`runReaderT` dirtyPlansRef)
            . (`evalMaybeT` ())
            $ rerenderDirtyPlans

    -- create an mvar to store the obj to be created
    v <- newEmptyMVar

    -- generate the commands that will create the obj
    cs <- execProgramT' . evalContT $ mkObj wid logname s >>= void . liftIO . tryPutMVar v

    -- run the commands to generate the obj
    let u = ((`runReaderT` dirtyPlansRef)
            . (`runReaderT` nextReactIdRef)
            . (`runReaderT` logConfigRef))

    u $ traverse_ execApp cs

    -- try render the obj
    runMaybeT $ do
        obj <- MaybeT $ tryTakeMVar v
        renderAndExportObj root obj





