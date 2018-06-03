{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Glazier.React.Reactor.Exec
    ( maybeExec
    , maybeExecReactor
    , execReactorCmd
    , execRerender
    , execMkSubject
    , execMkHandler
    , execMkHandler1
    , execGetScene
    , execTickScene
    ) where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.AExcept
import Control.Monad.Trans.ARWS.Strict
import Control.Monad.Trans.AState.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import qualified Data.JSString as J
import Data.Maybe
import Data.Semigroup
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
import Glazier.React.ReactId.Internal
import Glazier.React.Reactor
import Glazier.React.ReadIORef
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Subject.Internal
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Array as JA
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
--     -> AState (GetScene c s) ()
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
    , Has (Tagged ReactId (MVar Int)) r
    , MonadReader r m
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

execReactorCmd ::
    ( MonadUnliftIO m
    , AsReactor cmd
    , Has (Tagged ReactId (MVar Int)) r
    , MonadReader r m
    )
    => (cmd -> m ()) -> ReactorCmd cmd -> m ()
execReactorCmd exec c = case c of
    MkReactId n k -> execMkReactId n >>= (exec . k)
    SetRender sbj w -> execSetRender sbj w
    MkSubject wid s k -> execMkSubject exec wid s >>= (exec . k)
    Rerender sbj -> execRerender sbj
    GetScene sbj k -> execGetScene sbj >>= (exec . k)
    TickScene sbj tick -> execTickScene sbj tick >>= exec
    MkHandler c' k -> execMkHandler exec c' >>= (exec . k)
    MkHandler1 goStrict goLazy k -> execMkHandler1 exec goStrict goLazy >>= (exec . k)

-----------------------------------------------------------------
execMkReactId ::
    ( MonadIO m
    , Has (Tagged ReactId (MVar Int)) r
    , MonadReader r m
    )
    => J.JSString
    -> m ReactId
execMkReactId n = do
    v <- view (pieceTag' @ReactId)
    liftIO $ do
        i <- takeMVar v
        let i' = JE.safeIncrement i
        putMVar v i'
        pure . ReactId . J.append n . J.cons ':' . J.pack $ show i'


doRender :: IORef (Scene s) -> Window s () -> IO J.JSVal
doRender scnRef win = do
    -- render using from scnRef (doesn't block)
    scn <- readIORef scnRef
    (mrkup, _) <- unReadIORef (execARWST win scn mempty) -- ignore unit writer output
    JE.toJS <$> toElement mrkup

doRef :: IORef (Scene s) -> MVar (Scene s) -> J.JSVal -> IO ()
doRef scnRef scnVar j = do
    -- update componentRef held in the Plan
    Scene pln mdl <- takeMVar scnVar
    let scn' = Scene (pln & _componentRef .~ JE.fromJS j) mdl
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'

doRendered :: IORef (Scene s) -> MVar (Scene s) -> IO ()
doRendered scnRef scnVar = join $ do
    -- Get the IO actions doOnRendered and reset the "Once" actions
    Scene pln mdl <- takeMVar scnVar
    let ((untag @"Once") -> x, (untag @"Always") -> y) = doOnRendered pln
        scn' = Scene (pln & _doOnRendered._1 .~ (Tagged @"Once" mempty)) mdl
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    -- runs the "Once" actions before "Always".
    pure (x *> y)

-- Refer to 'Glazier.React.Window.getListeners'
doListen :: IORef (Scene s) -> MVar (Scene s) -> J.JSVal -> J.JSVal -> IO ()
doListen scnRef scnVar ctx j = void $ runMaybeT $ do
    -- ctx is [ ReactId, event name (eg OnClick) ]
    -- Javascript doesn't have tuples, only list
    (ri, n) <- MaybeT $ pure $ do
        ctx' <- JE.fromJS ctx
        case JA.toList ctx' of
                [ri', n'] -> do
                    ri'' <- ReactId <$> JE.fromJS ri'
                    n'' <- JE.fromJS n'
                    Just (ri'', n'')
                -- malformed ctx, ignore
                _ -> Nothing
    lift $ do
        mhdl <- do
                Scene pln mdl <- takeMVar scnVar
                -- get the handler for the elemental and event name.
                -- also get the updated elemental as the "Once" handler may be reset.
                -- returns (Maybe handler, Maybe newElemental)
                -- First, look for elemental:
                let (mhdl, gs') = at ri go (pln ^. _elementals)
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
                    scn' = Scene (pln & _elementals .~ gs') mdl
                atomicWriteIORef scnRef scn'
                putMVar scnVar scn'
                pure mhdl
        -- pass the javascript event arg into the combined handler
        case mhdl of
            Nothing -> pure ()
            Just hdl -> hdl (JE.JSRep j)


execSetRender :: MonadIO m => Subject s -> Window s () -> m ()
execSetRender (Subject scnRef scnVar _ renderLeaseRef) win = liftIO $ do
    -- create the callbacks
    renderCb <- J.syncCallback' (doRender scnRef win)
    -- Create automatic garbage collection of the callbacks
    -- that will run when the Subject lease members are garbage collected.
    renderLease <- liftIO $ newEmptyMVar
    void $ mkWeakMVar renderLease (CD.runDisposable $ CD.dispose renderCb)
    -- Replace the existing ShimCallbacks (was fake version)
    scn <- takeMVar scnVar
    -- replace the rendering function
    atomicWriteIORef renderLeaseRef renderLease
    let scn' = scn & _plan._shimCallbacks._shimRender .~ renderCb
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'

-- | Make an initialized 'Subject' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displaySubject' should be used to render the subject.
execMkSubject ::
    ( MonadIO m
    , AsReactor cmd
    )
    => (cmd -> m ())
    -> Widget cmd s s ()
    -> s
    -> m (Subject s)
execMkSubject exec wid s = do
    -- create shim with fake callbacks for now
    let scn = Scene newPlan s
    scnRef <- liftIO $ newIORef scn
    scnVar <- liftIO $ newEmptyMVar
    -- Create a MVar just for auto cleanup of the callbacks
    -- This mvar must not be reachable from the callbacks,
    -- otherwise the callbacks will always be alive.
    otherCallbackLease <- liftIO $ newEmptyMVar
    renderLease <- liftIO $ newEmptyMVar
    renderLeaseRef <- liftIO $ newIORef renderLease
    -- Create callbacks for now
    renderCb <- liftIO $ J.syncCallback' (pure J.nullRef) -- dummy render for now
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync (doRef scnRef scnVar)
    renderedCb <- liftIO $ J.syncCallback J.ContinueAsync (doRendered scnRef scnVar)
    listenCb <- liftIO $ J.syncCallback2 J.ContinueAsync (doListen scnRef scnVar)
    -- Create automatic garbage collection of the callbacks
    -- that will run when the Subject lease members are garbage collected.
    liftIO $ void $ mkWeakMVar otherCallbackLease (CD.runDisposable $
        CD.dispose refCb <> CD.dispose renderedCb <> CD.dispose listenCb)
    liftIO $ void $ mkWeakMVar renderLease (CD.runDisposable $ CD.dispose renderCb)
    -- Now we have enough to make a subject
    let sbj = Subject scnRef scnVar otherCallbackLease renderLeaseRef
        -- initalize the subject using the Gadget
        gad = runAExceptT wid
        gad' = gad `bindLeft` (postCmd' . SetRender sbj)
        gad'' = (either id id) <$> gad'
        tick = runGadget gad'' (Entity sbj id) pure
        cs = execAState tick mempty
        -- update the scene to include the real shimcallbacks
        scn' = scn & _plan._shimCallbacks .~ ShimCallbacks renderCb renderedCb refCb listenCb
    liftIO $ do
        -- update the mutable variables with the initialzed scene
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
    -- execute additional commands
    -- one of these commands will be 'SetRender' which will
    -- update the dummy render with the real render function.
    exec (command' $ DL.toList cs)
    -- return the subject
    pure sbj
  where
    newPlan = Plan
        Nothing
        (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
        (Tagged mempty, Tagged mempty)
        mempty

_rerender :: Scene s -> IO ()
_rerender scn = fromMaybe mempty $ rerenderShim <$> (scn ^. _plan._componentRef)

execRerender ::
    ( MonadIO m
    )
    => Subject s -> m ()
execRerender sbj = liftIO $ do
    scn <- readIORef $ sceneRef sbj
    _rerender scn

execGetScene ::
    MonadIO m
    => Subject s
    -> m (Scene s)
execGetScene sbj = liftIO . readIORef $ sceneRef sbj

-- | No need to run in a separate thread because it should never block for a significant amount of time.
-- Upate the scene 'MVar' with the given action. Also triggers a rerender.
execTickScene ::
    ( MonadIO m
    )
    => Subject s
    -> StateT (Scene s) ReadIORef cmd
    -> m cmd
execTickScene sbj tick = liftIO $ do
    scn <- takeMVar scnVar
    (c, scn') <- unReadIORef $ runStateT tick scn
    -- Update the back buffer
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    -- automatically rerender the scene
    _rerender scn'
    pure c
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj

execMkHandler1 ::
    (NFData a, MonadUnliftIO m)
    => (cmd -> m ())
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> m (JE.JSRep -> IO ())
execMkHandler1 exec goStrict goLazy = do
    UnliftIO u <- askUnliftIO
    let f = handleEventM goStrict goLazy'
        goLazy' = liftIO . u . exec . goLazy
    -- Apply to result to the continuation, and execute any produced commands
    pure $ (void . runMaybeT) <$> f

execMkHandler ::
    (MonadUnliftIO m)
    => (cmd -> m ())
    -> cmd
    -> m (IO ())
execMkHandler exec c = do
    UnliftIO u <- askUnliftIO
    let f = u $ exec c
    -- Apply to result to the continuation, and execute any produced commands
    pure f

