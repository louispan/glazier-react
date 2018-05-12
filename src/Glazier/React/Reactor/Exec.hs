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
import Control.Monad.Trans.ARWS.Strict
import Control.Monad.Trans.AState.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Tagged
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Command.Exec
import Glazier.React.Component
import Glazier.React.Entity
import Glazier.React.Gadget
import Glazier.React.HandleEvent
import Glazier.React.Markup
import Glazier.React.MkId.Internal
import Glazier.React.Reactor
import Glazier.React.Scene
import Glazier.React.Subject.Internal
import Glazier.React.Widget
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

-- maybeExec' :: (Monad m, AsFacet (c' c) c) => (c' c -> m b) -> c -> MaybeT m b
-- maybeExec' = maybeExec

displaySubject :: (MonadIO m, MonadState (DL.DList ReactMarkup) m) => Subject s -> m ()
displaySubject (Subject scnRef _ _) = do
    scn <- liftIO $ readIORef scnRef
    let ShimCallbacks renderCb renderedCb refCb _ = scn ^. _plan._shimCallbacks
    -- These are the callbacks on the 'ShimComponent'
    -- See jsbits/react.js
    leaf shimComponent
        [ ("render", JE.toJSR renderCb)
        , ("rendered", JE.toJSR renderedCb)
        , ("ref", JE.toJSR refCb)
        ]


-- | Make an initialized 'Subject' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displaySubject' should be used to render the subject.
mkSubject ::
    ( MonadIO m
    , AsFacet [cmd] cmd
    )
    => (cmd -> m ())
    -> Widget cmd s s ()
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
        doRendered = join $ do
            -- Get the IO actions doOnRendered and reset the "Once" actions
            Scene pln mdl <- takeMVar scnVar
            let ((untag @"Once") -> x, (untag @"Always") -> y) = doOnRendered pln
            putMVar scnVar $ Scene (pln & _doOnRendered._1 .~ (Tagged @"Once" mempty)) mdl
            -- runs the "Once" actions before "Always".
            pure (x *> y)
        doListen ctx j = void $ runMaybeT $ do
            -- ctx is [ ElementalId, event name (eg OnClick) ]
            -- Javascript doesn't have tuples, only list
            (eid, n) <- MaybeT $ pure $ do
                ctx' <- JE.fromJS ctx
                case JA.toList ctx' of
                        [eid', n'] -> do
                            eid'' <- ElementalId <$> JE.fromJS eid'
                            n'' <- JE.fromJS n'
                            Just (eid'', n'')
                        -- malformed ctx, ignore
                        _ -> Nothing
            lift $ do
                mhdl <- do
                        Scene pln mdl <- takeMVar scnVar
                        -- get the handler for the elemental and event name.
                        -- also get the updated elemental as the "Once" handler may be reset.
                        -- returns (Maybe handler, Maybe newElemental)
                        -- First, look for elemental:
                        let (mhdl, gs') = at eid go (pln ^. _elementals)
                            go mg = case mg of
                                    Nothing -> (Nothing, Nothing)
                                    -- found elemental, check listeners for event name
                                    Just g -> let (ret, l) = at n go' (g ^. _listeners)
                                              in (ret, Just (g & _listeners .~ l))
                            go' ml = case ml of
                                    Nothing -> (Nothing, Nothing)
                                    -- Found listener with event name, reset "Once" actions
                                    Just ((untag @"Once") -> x, y) ->
                                        ( Just (x *> (untag @"Always" y))
                                        , Just (Tagged @"Once" mempty, y))
                        -- update the elemental with resetted "Once" listeners
                        -- and return the combined handler
                        putMVar scnVar $ Scene (pln & _elementals .~ gs') mdl
                        pure mhdl
                -- pass the javascript event arg into the combined handler
                case mhdl of
                    Nothing -> pure ()
                    Just hdl -> hdl (JE.JSRep j)

    renderCb <- liftIO $ J.syncCallback' doRender
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync doRef
    renderedCb <- liftIO $ J.syncCallback J.ContinueAsync doRendered
    listenCb <- liftIO $ J.syncCallback2 J.ContinueAsync doListen
    -- Create a MVar just for auto cleanup of the callbacks
    -- This mvar must not be reachable from the callbacks,
    -- otherwise the callbacks will always be alive.
    rel <- liftIO $ newEmptyMVar
    let cbs = ShimCallbacks renderCb renderedCb refCb listenCb
        pln = newPlan & _shimCallbacks .~ cbs
        scn = Scene pln s
        -- keep zombie alive as long as 'Subject' 'prolong' is reachable.
        sbj = Subject scnRef scnVar rel
        -- initalize the subject using the Gadget
        tick = runGadgetT gad (Entity sbj id) (const $ pure ())
        cs = execAState tick mempty
        cleanup = CD.runDisposable $ CD.dispose cbs
    liftIO $ do
        -- Create automatic garbage collection of the callbacks
        -- that will run when the zombieVar is garbage collected.
        void $ mkWeakMVar rel cleanup
        -- update the mutable variables
        atomicWriteIORef scnRef scn
        putMVar scnVar scn
    -- execute additional commands
    exec (command' $ DL.toList cs)
    -- return the initialized subject
    pure sbj
  where
    newPlan :: Plan
    newPlan = Plan
        Nothing
        (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
        (Tagged mempty, Tagged mempty)
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
execRerender (Subject scnRef _ _) = liftIO $ do
    scn <- readIORef scnRef
    _rerender scn

-- | No need to run in a separate thread because it should never block for a significant amount of time.
-- Upate the scene 'MVar' with the given action. Also triggers a rerender.
execTickScene ::
    ( MonadIO m
    )
    => Subject s
    -> State (Scene s) ()
    -> m ()
execTickScene (Subject scnRef scnVar _) tick = liftIO $ do
    scn <- takeMVar scnVar
    let scn' = execState tick scn
    -- Update the back buffer
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    -- automatically rerender the scene
    _rerender scn'


execScenario ::
    ( MonadIO m
    , AsFacet [cmd] cmd
    )
    => (cmd -> m ())
    -> Subject s
    -> ReaderT (Scene s) (State (DL.DList cmd)) ()
    -> m ()
execScenario exec  (Subject scnRef _ _) go = do
    scn <- liftIO $ readIORef scnRef
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
