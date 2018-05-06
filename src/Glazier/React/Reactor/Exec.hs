{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Glazier.React.Reactor.Exec
    ( displaySubject
    , mkSubject
    , maybeExec
    , maybeExecReactor
    , execReactorCmd
    , execCommands
    , execDisposable
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
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Command.Exec
import Glazier.React.Component
import Glazier.React.Gadget
import Glazier.React.HandleEvent
import Glazier.React.Markup
import Glazier.React.MkId.Internal
import Glazier.React.Reactor
import Glazier.React.Scene
import Glazier.React.Widget
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

-- maybeExec' :: (Monad m, AsFacet (c' c) c) => (c' c -> m b) -> c -> MaybeT m b
-- maybeExec' = maybeExec

displaySubject :: (MonadIO m, MonadState (DL.DList ReactMarkup) m) => Subject s -> m ()
displaySubject (Subject scnRef _) = do
    scn <- liftIO $ readIORef scnRef
    let ShimCallbacks renderCb updatedCb refCb _ = scn ^. _plan._shimCallbacks
    -- These are the callbacks on the 'ShimComponent'
    -- See jsbits/react.js
    leaf shimComponent
        [ ("render", JE.toJSR renderCb)
        , ("updated", JE.toJSR updatedCb)
        , ("ref", JE.toJSR refCb)
        ]

-- displaySubject' :: WindowT (Subject s) IO ()
-- displaySubject' (Subject scnRef _) = do
--     scn <- liftIO $ readIORef scnRef
--     let ShimCallbacks renderCb updatedCb refCb _ = scn ^. _plan._shimCallbacks
--     -- These are the callbacks on the 'ShimComponent'
--     -- See jsbits/react.js
--     leaf shimComponent
--         [ ("render", JE.toJSR renderCb)
--         , ("updated", JE.toJSR updatedCb)
--         , ("ref", JE.toJSR refCb)
--         ]

-- | Make an initialized 'Subject' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displaySubject' should be used to render the subject.
mkSubject ::
    ( MonadIO m
    , AsFacet [c] c
    )
    => (c -> m ())
    -> Widget c s s ()
    -> s
    -> m (Subject s)
mkSubject exec (Widget win gad) s = do
    scnRef <- liftIO $ newIORef (Scene newPlan s)
    scnVar <- liftIO $ newEmptyMVar
    let doRender = do
            -- render using from scnRef (doesn't block)
            scn <- readIORef scnRef
            (mrkup, _) <- execARWST win scn mempty -- ignore unit writer output
            JE.toJS <$> toElement mrkup
        doRef j = do
            -- update componentRef held in the Plan
            Scene pln mdl <- takeMVar scnVar
            putMVar scnVar $ Scene (pln & _componentRef .~ JE.fromJS j) mdl
        doUpdated = join $ do
            -- Get the IO actions onUpdated and reset the "Once" actions
            Scene pln mdl <- takeMVar scnVar
            let ((`proxy` (Proxy @"Once")) -> x, (`proxy` (Proxy @"Every")) -> y) = doOnUpdated pln
            putMVar scnVar $ Scene (pln & _doOnUpdated._1 .~ (Tagged @"Once" mempty)) mdl
            -- runs the "Once" actions before "Every".
            pure (x *> y)
        doListen ctx j = void $ runMaybeT $ do
            -- ctx is [ GizmoId, event name (eg OnClick) ]
            -- Javascript doesn't have tuples, only list
            (gid, n) <- MaybeT $ pure $ do
                ctx' <- JE.fromJS ctx
                case JA.toList ctx' of
                        [gid', n'] -> do
                            gid'' <- GizmoId <$> JE.fromJS gid'
                            n'' <- JE.fromJS n'
                            Just (gid'', n'')
                        -- malformed ctx, ignore
                        _ -> Nothing
            lift $ do
                mhdl <- do
                        Scene pln mdl <- takeMVar scnVar
                        -- get the handler for the gizmo and event name.
                        -- also get the updated gizmo as the "Once" handler may be reset.
                        -- returns (Maybe handler, Maybe newGizmo)
                        -- First, look for gizmo:
                        let (mhdl, gs') = at gid go (pln ^. _gizmos)
                            go mg = case mg of
                                    Nothing -> (Nothing, Nothing)
                                    -- found gizmo, check listeners for event name
                                    Just g -> let (ret, l) = at n go' (g ^. _listeners)
                                              in (ret, Just (g & _listeners .~ l))
                            go' ml = case ml of
                                    Nothing -> (Nothing, Nothing)
                                    -- Found listener with event name, reset "Once" actions
                                    Just ((`proxy` (Proxy @"Once")) -> x, y) ->
                                        ( Just (x *> (y `proxy` (Proxy @"Every")))
                                        , Just (Tagged @"Once" mempty, y))
                        -- update the gizmo with resetted "Once" listeners
                        -- and return the combined handler
                        putMVar scnVar $ Scene (pln & _gizmos .~ gs') mdl
                        pure mhdl
                -- pass the javascript event arg into the combined handler
                case mhdl of
                    Nothing -> pure ()
                    Just hdl -> hdl (JE.JSRep j)

    renderCb <- liftIO $ J.syncCallback' doRender
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync doRef
    updatedCb <- liftIO $ J.syncCallback J.ContinueAsync doUpdated
    listenCb <- liftIO $ J.syncCallback2 J.ContinueAsync doListen
    let pln = newPlan & _shimCallbacks .~ (ShimCallbacks renderCb updatedCb refCb listenCb)
        scn = Scene pln s
        sbj = Subject scnRef scnVar
        -- initalize the subject using the Gadget
        tick = runGadgetT gad (Entity sbj id) (const $ pure ())
        Scenario cs scn' = execAState tick (Scenario mempty scn)
    liftIO $ atomicWriteIORef scnRef scn'
    liftIO $ putMVar scnVar scn'
    -- execute additional commands
    exec (stamp' $ DL.toList cs)
    -- return the initialized subject
    pure sbj
  where
    newPlan :: Plan
    newPlan = Plan
        Nothing
        (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
        (Tagged mempty, Tagged mempty)
        mempty
        mempty

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

-- | Upate the world 'TVar' with the given action, and return the commands produced.
_tickState :: Subject s -> AState (Scenario c s) () -> IO (DL.DList c)
_tickState (Subject scnRef scnVar) tick = do
    -- | 'tickState' is the only place where we 'takeMVar' the plan or model
    -- So as long as we take and put in the correct order, we won't block.
    scn <- takeMVar scnVar
    let (Scenario cs scn') = execAState tick (Scenario mempty scn)

    -- Update the back buffer
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    -- rerender if necessary
    _rerender scn'
    pure cs

_rerender :: Scene s -> IO ()
_rerender scn = fromMaybe mempty $ rerenderShim <$> (scn ^. _plan._componentRef)

-- | Create a executor for all the core commands required by the framework
maybeExecReactor ::
    ( MonadUnliftIO m
    , AsReactor c
    )
    => (c -> m ()) -> c -> MaybeT m ()
maybeExecReactor exec c =
    maybeExec (traverse_ @[] exec) c
    <|> maybeExec (execReactorCmd exec) c
    <|> maybeExec execDisposable c

execReactorCmd ::
    ( MonadUnliftIO m
    , AsFacet [c] c
    )
    => (c -> m ()) -> ReactorCmd c -> m ()
execReactorCmd exec c = case c of
    Rerender sbj -> execRerender sbj
    TickState sbj tick -> execTickState exec sbj tick
    MkAction c' k -> execMkAction exec c' k
    MkAction1 goStrict goLazy k -> execMkAction1 exec goStrict goLazy k
    -- MkShimCallbacks sbj rndr -> execMkShimCallbacks sbj rndr
    MkSubject wid s k -> execMkSubject exec wid s k

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

-- execute a list of commands in serial, not in parallel because
-- Some commands may have effect dependencies.
-- Javascript is single threaded anyway, so it is more
-- efficient to execuute "quick" commands single threaded.
-- We'll let the individual executors of the commands decide if
-- "slow" commands should be forked in a thread.
execCommands :: Applicative m => (c -> m ()) -> [c] -> m ()
execCommands exec = traverse_ exec

execRerender ::
    ( MonadIO m
    )
    => Subject s -> m ()
execRerender (Subject scnRef _) = liftIO $ do
    scn <- readIORef scnRef
    _rerender scn

-- | No need to run in a separate thread because it should never block for a significant amount of time.
execTickState ::
    ( MonadIO m
    , AsFacet [c] c
    )
    => (c -> m ())
    -> Subject s
    -> (AState (Scenario c s) ())
    -> m ()
execTickState exec sbj tick = do
    cs <- liftIO $ _tickState sbj tick
    exec (stamp' $ DL.toList cs)

execMkAction1 ::
    (NFData a, MonadUnliftIO m)
    => (c -> m ())
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> c)
    -> ((JE.JSRep -> IO ()) -> c)
    -> m ()
execMkAction1 exec goStrict goLazy k = do
    UnliftIO u <- askUnliftIO
    let f = handleEventM goStrict goLazy'
        goLazy' ma = case ma of
            -- trigger didn't produce anything useful
            Nothing -> pure mempty
            -- get and run the command given the trigger
            Just a -> u . exec $ goLazy a
    -- Apply to result to the continuation, and execute any produced commands
    exec $ k f

execMkAction ::
    (MonadUnliftIO m)
    => (c -> m ())
    -> c
    -> (IO () -> c)
    -> m ()
execMkAction  exec c k = do
    UnliftIO u <- askUnliftIO
    let f = u $ exec c
    -- Apply to result to the continuation, and execute any produced commands
    exec $ k f

execMkSubject ::
    ( MonadIO m
    , AsFacet [c] c
    )
    => (c -> m ())
    -> Widget c s s ()
    -> s
    -> (Subject s -> c)
    -> m ()
execMkSubject exec wid s k = do
    sbj <- mkSubject exec wid s
    exec $ k sbj

execDisposable ::
    MonadIO m
    => CD.Disposable
    -> m ()
execDisposable = liftIO . fromMaybe mempty . CD.runDisposable
