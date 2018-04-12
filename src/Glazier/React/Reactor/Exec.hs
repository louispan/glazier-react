{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Glazier.React.Reactor.Exec where

import Control.Applicative
import Control.Concurrent
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.ARWS.Strict
import Control.Monad.Trans.AState.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Proxy
import Data.Tagged
import qualified GHCJS.Foreign.Callback as J
import Glazier.React.Component
import Glazier.React.HandleEvent
import Glazier.React.Markup
import Glazier.React.MkId.Internal
import Glazier.React.Reactor
import Glazier.React.Scene
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

maybeExec :: (Monad m, AsFacet a c) => (a -> m b) -> c -> MaybeT m b
maybeExec k y = maybe empty pure (preview facet y) >>= (lift <$> k)

-- maybeExec' :: (Monad m, AsFacet (c' c) c) => (c' c -> m b) -> c -> MaybeT m b
-- maybeExec' = maybeExec

-- | Given an initializing state action, and an executor (eg. 'reactorExecutor'),
-- initialize and exec the resulting commands generated, then
-- return the 'CD.Disposable' to dispose the handlers created,
-- as well as an action to read the current state.
-- It is the responsiblity of the caller run the 'CD.Disposable' when the app finishes.
initReactor ::
    ( MonadIO m
    , AsFacet [c] c
    )
    => (c -> m ())
    -> s
    -> AState (Scenario c s) ()
    -> m (CD.Disposable, IO s)
initReactor exec s ini = do
    frmRef <- liftIO $ newIORef (Scene newPlan s)
    plnVar <- liftIO $ newMVar newPlan
    mdlVar <- liftIO $ newMVar s
    -- run through the app initialization, and execute any produced commands
    cs <- liftIO $ tickState (Subject frmRef plnVar mdlVar) ini
    exec (cmd' $ DL.toList cs)
    pure (CD.dispose plnVar, readMVar mdlVar)

-- | Upate the world 'TVar' with the given action, and return the commands produced.
tickState :: Subject s -> AState (Scenario c s) () -> IO (DL.DList c)
tickState (Subject scnRef plnVar mdlVar) tick = do
    mdl <- takeMVar mdlVar
    -- execShimCallbacks may 'takeMVar' only the plan,
    -- also plnVar may be shared with other 'Arena's
    -- so use plnVar inside mdVar block to release the plnVar as quickly as possible.
    pln <- takeMVar plnVar
    let (Scenario cs (Scene pln' mdl')) = execAState tick (Scenario mempty (Scene pln mdl))
        (rndr, pln'') = runState rerender pln'

    -- Update the back buffer
    writeIORef scnRef (Scene pln'' mdl')
    putMVar plnVar pln''
    putMVar mdlVar mdl'
    -- rerender if necessary
    rndr
    pure cs

rerender :: MonadState Plan m => m (IO ())
rerender = do
    c <- use (_currentFrameNum)
    p <- use (_previousFrameNum)
    comp <- use (_componentRef)
    -- we are dirty
    case (c /= p, comp) of
        (True, Just j) -> do
            _previousFrameNum .= c
            pure (js_setShimComponentFrameNum j c)
        _ -> pure (pure ())

-- type Wack w = Which '[Rerender, (), DL.DList w]
-- newtype Wock = Wock { unWock :: Wack Wock}
-- instance (AsFacet a (Wack Wock)) => AsFacet a Wock where
--     facet = iso unWock Wock . facet

-- | Create a executor for all the core commands required by the framework
execReactor ::
    ( MonadIO m
    , AsReactor c
    )
    => (m () -> IO ()) -> (c -> m ()) -> c -> MaybeT m ()
execReactor runExec exec c =
    maybeExec (execCommands runExec exec) c
    <|> maybeExec execRerender c
    <|> maybeExec (execTickState exec) c
    <|> maybeExec (execMkAction1 runExec exec) c
    <|> maybeExec (execMkAction runExec exec) c
    <|> maybeExec execMkShimCallbacks c
    <|> maybeExec execDisposable c

-- -- | An example of using the "tieing" 'execReactor' with itself. Lazy haskell is awesome.
-- -- NB. This tied executor *only* runs the Reactor effects.
-- -- You would probably want to "tie" at least 'execReactor', 'execJavascript', 'execHtmlElement'
-- reactorExecutor ::
--     ( MonadIO m
--     , AsReactor c
--     )
--     => (m () -> IO ()) -> c -> m ()
-- reactorExecutor runExec = fmap (fromMaybe mempty) . runMaybeT .
--     execReactor runExec (reactorExecutor runExec)

-----------------------------------------------------------------

-- execte a list of commands in serial.
-- This is because Javascript is single threaded anyway, so it is more
-- efficient to execuute "quick" commands single threaded.
-- We'll let the individual executors of the commands decide if
-- "slow" commands should be forked in a thread.
execCommands :: MonadIO m => (m () -> IO ()) -> (c -> m ()) -> [c] -> m ()
execCommands runExec exec = traverse_ (liftIO . runExec . exec)
-- execCommands runExec exec = traverse_ (liftIO . void . forkIO . runExec . exec)

execRerender ::
    ( MonadIO m
    )
    => Rerender -> m ()
execRerender (Rerender (Subject scnRef plnVar mdlVar)) = liftIO $ do
    -- | 'tickState' and 'execRerender' are the only place where we 'takeMVar' the plan or model
    -- So as long as we take and put in the correct order, we won't block.
    mdl <- readMVar mdlVar
    pln <- readMVar plnVar
    let (rndr, pln') = runState rerender pln
    writeIORef scnRef (Scene pln' mdl)
    putMVar plnVar pln'
    putMVar mdlVar mdl
    rndr

-- | No need to run in a separate thread because it should never block for a significant amount of time.
execTickState ::
    ( MonadIO m
    , AsFacet [c] c
    )
    => (c -> m ())
    -> TickState c
    -> m ()
execTickState exec (TickState sbj tick) = do
    cs <- liftIO $ tickState sbj tick
    exec (cmd' $ DL.toList cs)

execMkAction1 ::
    (m () -> IO ())
    -> (c -> m ())
    -> MkAction1 c
    -> m ()
execMkAction1 runExec exec (MkAction1 goStrict goLazy k) = do
    -- create the IO action to run given the runExec and exec
    let f = handleEventM goStrict goLazy'
        goLazy' ma = case ma of
            -- trigger didn't produce anything useful
            Nothing -> pure mempty
            -- get and run the command given the trigger
            Just a -> runExec . exec $ goLazy a
    -- Apply to result to the continuation, and execute any produced commands
    exec $ k f

execMkAction ::
    (m () -> IO ())
    -> (c -> m ())
    -> MkAction c
    -> m ()
execMkAction runExec exec (MkAction c k) = do
    -- create the IO action to run given the runExec and exec
    let f = runExec $ exec c
    -- Apply to result to the continuation, and execute any produced commands
    exec $ k f

-- | Making multiple MkShimListeners for the same plan is a silent error and will be ignored.
execMkShimCallbacks ::
    MonadIO m
    => MkShimCallbacks
    -> m ()
execMkShimCallbacks (MkShimCallbacks (Subject scnRef plnVar _) rndr) = do
    -- For efficiency, render uses the state exported into ShimComponent
    let doRender = do
            scn <- readIORef scnRef
            let (mrkup, _) = execARWS rndr scn mempty
            JE.toJS <$> toElement mrkup
        doRef j = do
            pln <- takeMVar plnVar
            putMVar plnVar (pln & _componentRef .~ JE.fromJS j)
        doUpdated = join $ do
            pln <- takeMVar plnVar
            let ((`proxy` (Proxy @"Once")) -> x, (`proxy` (Proxy @"Every")) -> y) = doOnUpdated pln
            putMVar plnVar $ pln & _doOnUpdated._1 .~ (Tagged @"Once" mempty)
            pure (x *> y)
        doListen ctx j = void $ runMaybeT $ do
            (gid, n) <- MaybeT $ pure $ do
                ctx' <- JE.fromJS ctx
                case JA.toList ctx' of
                        [gid', n'] -> do
                            gid'' <- GizmoId <$> JE.fromJS gid'
                            n'' <- JE.fromJS n'
                            Just (gid'', n'')
                        _ -> Nothing
            lift $ do
                mhdl <- do
                        pln <- takeMVar plnVar
                        let (mhdl, gs') = at gid go (pln ^. _gizmos)
                            go mg = case mg of
                                    Nothing -> (Nothing, Nothing)
                                    Just g -> let (ret, l) = at n go' (g ^. _listeners)
                                              in (ret, Just (g & _listeners .~ l))
                            go' ml = case ml of
                                    Nothing -> (Nothing, Nothing)
                                    Just ((`proxy` (Proxy @"Once")) -> x, y) ->
                                        ( Just (x *> (y `proxy` (Proxy @"Every")))
                                        , Just (Tagged @"Once" mempty, y))
                        putMVar plnVar (pln & _gizmos .~ gs')
                        pure mhdl
                case mhdl of
                    Nothing -> pure ()
                    Just hdl -> hdl (JE.JSRep j)

    renderCb <- liftIO $ J.syncCallback' doRender
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync doRef
    updatedCb <- liftIO $ J.syncCallback J.ContinueAsync doUpdated
    listenCb <- liftIO $ J.syncCallback2 J.ContinueAsync doListen
    liftIO $ do
        pln <- takeMVar plnVar
        let ls = pln ^. _shimCallbacks
        case ls of
            -- shim listeners already created
            Just _ -> putMVar plnVar pln
            Nothing -> putMVar plnVar $ pln & _shimCallbacks .~
                (Just $ ShimCallbacks renderCb updatedCb refCb listenCb)

execDisposable ::
    MonadIO m
    => CD.Disposable
    -> m ()
execDisposable = liftIO . fromMaybe mempty . CD.runDisposable

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1.setState({frameNum: $2}); }"
  js_setShimComponentFrameNum :: ComponentRef -> Int -> IO ()

#else

js_setShimComponentFrameNum :: ComponentRef -> Int -> IO ()
js_setShimComponentFrameNum _ _ = pure ()

#endif
