{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Reactor.Exec
    ( maybeExec
    , maybeExecReactor
    , execReactorCmd
    -- , execCommands
    -- , execDisposable
    ) where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Maybe
import Glazier.Command
import Glazier.Command.Exec
import Glazier.React.Component
import Glazier.React.HandleEvent
import Glazier.React.Reactor
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Widget
import qualified JavaScript.Extras as JE

-- -- | Given an initializing state action, and an executor (eg. 'reactorExecutor'),
-- -- initialize and exec the resulting commands generated, then
-- -- return the 'CD.Disposable' to dispose the handlers created,
-- -- as well as an action to read the current state.
-- -- It is the responsiblity of the caller run the 'CD.Disposable' when the app finishes.
-- initialize' ::
--     ( MonadIO m
--     , AsFacet [c] c
--     , CD.Dispose (Scene s)
--     )
--     => (c -> m ())
--     -> s
--     -> AState (Scenario c s) ()
--     -> m (CD.Disposable, IO s)
-- initialize' exec s ini = do
--     scnRef <- liftIO $ newIORef (Scene newPlan s)
--     scnVar <- liftIO $ newMVar (Scene newPlan s)
--     -- run through the app initialization, and execute any produced commands
--     cs <- liftIO $ _tickState (Subject scnRef scnVar) ini
--     exec (cmd' $ DL.toList cs)
--     pure (CD.dispose scnVar, view _model <$> readMVar scnVar)

-- initialize ::
--     ( MonadIO m
--     , AsFacet [c] c
--     , CD.Dispose (Scene s)
--     )
--     => (c -> m ())
--     -> Widget c p s a
--     -> s
--     -> m (CD.Disposable, IO s)
-- initialize exec s ini = do
--     scnRef <- liftIO $ newIORef (Scene newPlan s)
--     scnVar <- liftIO $ newMVar (Scene newPlan s)
--     -- run through the app initialization, and execute any produced commands
--     cs <- liftIO $ _tickState (Subject scnRef scnVar) ini
--     exec (cmd' $ DL.toList cs)
--     pure (CD.dispose scnVar, view _model <$> readMVar scnVar)

-- | Create a executor for all the core commands required by the framework
maybeExecReactor ::
    ( MonadUnliftIO m
    , AsReactor cmd
    )
    => (cmd -> m ()) -> cmd -> MaybeT m ()
maybeExecReactor exec c =
    -- execute a list of commands in serial, not in parallel because
    -- Some commands may have effect dependencies.
    -- Javascript is single threaded anyway, so it is more
    -- efficient to execuute "quick" commands single threaded.
    -- We'll let the individual executors of the commands decide if
    -- "slow" commands should be forked in a thread.
    maybeExec (traverse_ @[] exec) c
    <|> maybeExec (execReactorCmd exec) c
    <|> maybeExec (liftIO . CD.runDisposable) c

execReactorCmd ::
    ( MonadUnliftIO m
    , AsFacet [cmd] cmd
    )
    => (cmd -> m ()) -> ReactorCmd cmd -> m ()
execReactorCmd exec c = case c of
    -- MkShimCallbacks sbj rndr -> execMkShimCallbacks sbj rndr
    Rerender sbj -> execRerender sbj
    MkSubject wid s k -> execMkSubject exec wid s k
    MkHandler c' k -> execMkHandler exec c' k
    MkHandler1 goStrict goLazy k -> execMkHandler1 exec goStrict goLazy k
    Scenario sbj go -> execScenario exec sbj go
    TickScene sbj tick -> execTickScene sbj tick

-----------------------------------------------------------------

_rerender :: Scene s -> IO ()
_rerender scn = fromMaybe mempty $ rerenderShim <$> (scn ^. _plan._componentRef)

execRerender ::
    ( MonadIO m
    )
    => Subject s -> m ()
execRerender sbj = liftIO $ do
    scn <- readIORef $ sceneRef sbj
    _rerender scn

-- | No need to run in a separate thread because it should never block for a significant amount of time.
-- Upate the scene 'MVar' with the given action. Also triggers a rerender.
execTickScene ::
    ( MonadIO m
    )
    => Subject s
    -> State (Scene s) ()
    -> m ()
execTickScene sbj tick = liftIO $ do
    scn <- takeMVar scnVar
    let scn' = execState tick scn
    -- Update the back buffer
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    -- automatically rerender the scene
    _rerender scn'
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj

execScenario ::
    ( MonadIO m
    , AsFacet [cmd] cmd
    )
    => (cmd -> m ())
    -> Subject s
    -> ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> m ()
execScenario exec sbj go = do
    scn <- liftIO . readIORef $ sceneRef sbj
    let cs = (`execState` mempty) $ (`runReaderT` scn) go
    exec (command' $ DL.toList cs)

execMkHandler1 ::
    (NFData a, MonadUnliftIO m)
    => (cmd -> m ())
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> ((JE.JSRep -> IO ()) -> cmd)
    -> m ()
execMkHandler1 exec goStrict goLazy k = do
    UnliftIO u <- askUnliftIO
    let f = handleEventM goStrict goLazy'
        goLazy' = liftIO . u . exec . goLazy
    -- Apply to result to the continuation, and execute any produced commands
    exec . k $ (void . runMaybeT) <$> f

execMkHandler ::
    (MonadUnliftIO m)
    => (cmd -> m ())
    -> cmd
    -> (IO () -> cmd)
    -> m ()
execMkHandler  exec c k = do
    UnliftIO u <- askUnliftIO
    let f = u $ exec c
    -- Apply to result to the continuation, and execute any produced commands
    exec $ k f

execMkSubject ::
    ( MonadIO m
    , AsFacet [cmd] cmd
    )
    => (cmd -> m ())
    -> Widget cmd s s ()
    -> s
    -> (Subject s -> cmd)
    -> m ()
execMkSubject exec wid s k = do
    sbj <- mkSubject exec wid s
    exec $ k sbj
