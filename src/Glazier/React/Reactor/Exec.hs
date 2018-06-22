{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Reactor.Exec
    ( execReactorCmd
    , execRerender
    , execMkSubject
    , execGetScene
    , execTickScene
    , execRegisterReactListener
    , execRegisterRenderedListener
    , execRegisterDOMListener
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Maybe.Extras
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import qualified Data.JSString as J
import Data.Maybe
import Data.Tagged
import Data.Typeable
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Component
import Glazier.React.Entity
import Glazier.React.EventTarget
import Glazier.React.Gadget
import Glazier.React.Markup
import Glazier.React.ReactId.Internal
import Glazier.React.Reactor
import Glazier.React.ReadIORef
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Subject.Internal
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE

-- -- | Create a executor for all the core commands required by the framework
-- maybeExecReactor ::
--     ( MonadUnliftIO m
--     , AsReactor cmd
--     , Has (Tagged ReactId (MVar Int)) r
--     , MonadReader r m
--     )
--     => (cmd -> m ()) -> cmd -> MaybeT m ()
-- maybeExecReactor exec c =
--     -- execute a list of commands in serial, not in parallel because
--     -- Some commands may have effect dependencies.
--     -- Javascript is single threaded anyway, so it is more
--     -- efficient to execuute "quick" commands single threaded.
--     -- We'll let the individual executors of the commands decide if
--     -- "slow" commands should be forked in a thread.
--     maybeExec (traverse_ @[] exec) c
--     <|> maybeExec (execReactorCmd exec) c

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
    RegisterDOMListener sbj j n goStrict goLazy -> execRegisterDOMListener exec sbj j n goStrict goLazy
    RegisterReactListener sbj ri n goStrict goLazy -> execRegisterReactListener exec sbj ri n goStrict goLazy
    RegisterMountedListener sbj c' -> execRegisterMountedListener exec sbj c'
    RegisterRenderedListener sbj c' -> execRegisterRenderedListener exec sbj c'
    RegisterNextRenderedListener sbj c' -> execRegisterNextRenderedListener exec sbj c'
    RegisterTickedListener sbj c' -> execRegisterTickedListener exec sbj c'
    GetEventTarget sbj ri k -> (`evalMaybeT` ()) $ execGetEventTarget sbj ri >>= (lift . exec . k)

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
    putStrLn "doRender start"
    scn <- readIORef scnRef
    (mrkup, _) <- unReadIORef (execRWST win scn mempty) -- ignore unit writer output
    a <- JE.toJS <$> toElement mrkup
    putStrLn "doRender end"
    pure a

doRef :: IORef (Scene s) -> MVar (Scene s) -> J.JSVal -> IO ()
doRef scnRef scnVar j = do
    -- update componentRef held in the Plan
    scn <- takeMVar scnVar
    putStrLn "doRef start"
    let scn' = scn & _plan._componentRef .~ (JE.fromJS j)
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    putStrLn "doRef end"

doRendered :: IORef (Scene s) -> MVar (Scene s) -> IO ()
doRendered scnRef scnVar = do
    -- update nextRenderedListener held in the Plan
    scn <- takeMVar scnVar
    let scn' = scn & _plan._nextRenderedListener .~ mempty
        nxt = scn ^. _plan._nextRenderedListener
        (reentrancyGuard, cb) = scn ^. _plan._renderedListener

    putStrLn "doRendered start"
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    nxt
    -- only process _renderedListener if we are not already inside a renderedListener callstack.
    g <- atomically $ tryTakeTMVar reentrancyGuard
    case g of
        Nothing -> pure ()
        Just _ -> do
            cb
            atomically $ putTMVar reentrancyGuard ()
    putStrLn "doRendered end"

doMounted :: IORef (Scene s) -> IO ()
doMounted scnRef = do
    scn <- readIORef scnRef
    let (reentrancyGuard, cb) = scn ^. _plan._mountedListener
    putStrLn "doMounted start"
    -- only process _renderedListener if we are not already inside a renderedListener callstack.
    g <- atomically $ tryTakeTMVar reentrancyGuard
    case g of
        Nothing -> pure ()
        Just _ -> do
            cb
            atomically $ putTMVar reentrancyGuard ()
    putStrLn "doMounted end"

execSetRender :: MonadIO m => Subject s -> Window s () -> m ()
execSetRender sbj win = liftIO $ do
    -- create the callbacks
    putStrLn "execSetRender start"
    renderCb <- J.syncCallback' (doRender scnRef win)
    -- Create automatic garbage collection of the callbacks
    -- that will run when the Subject lease members are garbage collected.
    renderLease <- liftIO $ newEmptyMVar
    void $ mkWeakMVar renderLease $ J.releaseCallback renderCb
    -- Replace the existing ShimCallbacks (was fake version)
    scn <- takeMVar scnVar
    -- replace the rendering function
    atomicWriteIORef rndrLeaseRef renderLease
    let scn' = scn & _plan._shimCallbacks._shimRender .~ renderCb
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    putStrLn "execSetRender end"
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj
    rndrLeaseRef = renderLeaseRef sbj

-- | Make an initialized 'Subject' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displaySubject' should be used to render the subject.
execMkSubject ::
    ( MonadIO m
    , AsReactor cmd
    , Has (Tagged ReactId (MVar Int)) r
    , MonadReader r m
    )
    => (cmd -> m ())
    -> Widget cmd s s ()
    -> s
    -> m (Subject s)
execMkSubject exec wid s = do
    ri <- execMkReactId (J.pack "plan")
    (sbj, cs) <- liftIO $ do
        -- create shim with fake callbacks for now
        tickedGuard <- newTMVarIO ()
        mountedGuard <- newTMVarIO ()
        renderedGuard <- newTMVarIO ()
        let newPlan = Plan
                ri
                Nothing
                (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
                (tickedGuard, mempty)
                (mountedGuard, mempty)
                (renderedGuard, mempty)
                mempty
                mempty
                mempty
                mempty
            scn = Scene newPlan s

        scnRef <- newIORef scn
        scnVar <- newEmptyMVar
        -- Create a MVar just for auto cleanup of the callbacks
        -- This mvar must not be reachable from the callbacks,
        -- otherwise the callbacks will always be alive.
        otherCbLease <- newEmptyMVar
        renderLease <- newEmptyMVar
        rndrLeaseRef <- newIORef renderLease

        -- Create callbacks for now
        renderCb <- J.syncCallback' (pure J.nullRef) -- dummy render for now
        refCb <- J.syncCallback1 J.ContinueAsync (doRef scnRef scnVar)
        mountedCb <- J.syncCallback J.ContinueAsync (doMounted scnRef)
        renderedCb <- J.syncCallback J.ContinueAsync (doRendered scnRef scnVar)
        -- Create automatic garbage collection of the callbacks
        -- that will run when the Subject lease members are garbage collected.
        void $ mkWeakMVar otherCbLease $ do
            scn' <- readIORef scnRef
            scn' ^. _plan._nextRenderedListener
            scn' ^. _plan._finalCleanup
            -- cleanup callbacks
            traverse_ (traverse (J.releaseCallback . fst) . reactListeners) (scn' ^. _plan._elementals)
            traverse_ (J.releaseCallback . fst) (scn' ^. _plan._domlListeners)
            J.releaseCallback refCb
            J.releaseCallback renderedCb
        void $ mkWeakMVar renderLease $ J.releaseCallback renderCb

        -- Now we have enough to make a subject
        let sbj = Subject scnRef scnVar rndrLeaseRef otherCbLease
            -- initalize the subject using the Gadget
            gad = runExceptT wid
            gad' = gad `bindLeft` (postCmd' . SetRender sbj)
            gad'' = (either id id) <$> gad'
            tick = runGadget gad'' (Entity sbj id) pure
            cs = execState tick mempty
            -- update the scene to include the real shimcallbacks
            scn' = scn & _plan._shimCallbacks .~ ShimCallbacks renderCb mountedCb renderedCb refCb
        -- update the mutable variables with the initialzed scene
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        pure (sbj, cs)
    -- execute additional commands
    -- one of these commands will be 'SetRender' which will
    -- update the dummy render with the real render function.
    exec (command' $ DL.toList cs)
    -- return the subject
    pure sbj

_rerender :: Typeable s => Scene s -> IO ()
_rerender scn = case scn ^. _plan._componentRef of
    Nothing -> putStrLn $ "can't rerender - no componentRef: " ++ show (typeOf scn)
    Just j -> rerenderShim j

execRerender ::
    ( MonadIO m, Typeable s
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

execGetEventTarget ::
    MonadIO m
    => Subject s
    -> ReactId
    -> MaybeT m (EventTarget)
execGetEventTarget sbj ri = MaybeT $ liftIO $
    (preview (_plan._elementals.ix ri._elementalRef._Just)) <$> (readIORef $ sceneRef sbj)

-- | No need to run in a separate thread because it should never block for a significant amount of time.
-- Upate the scene 'MVar' with the given action. Also triggers a rerender.
execTickScene ::
    ( MonadIO m, Typeable s
    )
    => Subject s
    -> StateT (Scene s) ReadIORef cmd
    -> m cmd
execTickScene sbj tick = liftIO $ do
    scn <- takeMVar scnVar
    let (reentrancyGuard, cb) = scn ^. _plan._tickedListener
    putStrLn $ "execTickScene start: " ++ show (typeOf scn)
    (c, scn') <- unReadIORef $ runStateT tick scn
    -- Update the back buffer
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'

    -- only process _tickedListener if we are not already inside an tickedListener callstack.
    g <- atomically $ tryTakeTMVar reentrancyGuard
    case g of
        Nothing -> pure ()
        Just _ -> do
            putStrLn "ticked"
            cb
            atomically $ putTMVar reentrancyGuard ()

    -- automatically rerender the scene after state change
    _rerender scn'
    putStrLn $ "execTickScene end: " ++ show (typeOf scn)
    pure c
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj

mkEventCallback ::
    (MonadIO m)
    => IORef (J.JSVal -> IO (), IO ())
    -> m (J.Callback (J.JSVal -> IO ()))
mkEventCallback hdlRef = do
    liftIO $ J.syncCallback1 J.ContinueAsync $ \evt -> do
        (preprocessor, postprocessor) <- readIORef hdlRef
        -- first run all the non-blocking preprocessors
        preprocessor evt
        -- then run all the possibly blocking postprocessors
        postprocessor

-- | Using the NFData idea from React/Flux/PropertiesAndEvents.hs
-- React re-uses Notice from a pool, which means it may no longer be valid if we lazily
-- parse it. However, we still want lazy parsing so we don't parse unnecessary fields.
-- Additionally, we don't want to block during the event handling.The reason this is a problem is
-- because Javascript is single threaded, but Haskell is lazy.
-- Therefore GHCJS threads are a strange mixture of synchronous and asynchronous threads,
-- where a synchronous thread might be converted to an asynchronous thread if a "black hole" is encountered.
-- See https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Concurrent.hs
-- This safe interface requires two input functions:
-- 1. a function to reduce Notice to a NFData. The handleEvent will ensure that the
-- NFData is forced which will ensure all the required fields from Synthetic event has been parsed.
-- This function must not block.
-- 2. a second function that uses the NFData. This function is allowed to block.
-- handleEvent results in a function that you can safely pass into 'GHC.Foreign.Callback.syncCallback1'
-- with 'GHCJS.Foreign.Callback.ContinueAsync'.
-- I have innovated further with the NFData idea to return two functions:
-- 1. (evt -> IO ()) function to preprocess the event, which is guaranteed to be non blocking.
-- 2. An IO () postprocessor function which may block.
-- This allows for multiple handlers for the same event to be processed safely,
-- by allowing a way for all the preprocessor handlers are all run first before
-- running all of the postprocessor handlers.
mkEventHandler :: (NFData a) => (evt -> MaybeT IO a) -> IO (evt -> IO (), MaybeT IO a)
mkEventHandler goStrict = do
    -- create a channel to write preprocessed data for the postprocessor
    -- 'Chan' guarantees that the writer is never blocked by the reader.
    -- There is only one reader/writer per channel.
    c <- newTChanIO
    let preprocess evt = (`evalMaybeT` ()) $ do
            r <- goStrict evt
            -- This is guaranteed never to block
            lift $ atomically $ writeTChan c $!! r
        -- there might not be a value in the chan
        -- because the preprocessor might not have produced any values
        postprocess = MaybeT $ atomically $ tryReadTChan c
    pure (preprocess, postprocess)

execRegisterReactListener :: (NFData a, MonadUnliftIO m)
    => (cmd -> m ())
    -> Subject s
    -> ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> m ()
execRegisterReactListener exec sbj ri n goStrict goLazy = do
    UnliftIO u <- askUnliftIO
    liftIO $ do
        scn <- takeMVar scnVar
        -- first get or make the target
        eventHdl <-
            case scn ^. _plan._elementals.at ri.to (fromMaybe (Elemental Nothing mempty))._reactListeners.at n of
                Nothing -> do
                    listenerRef <- newIORef mempty
                    addEventListener (u . exec) goStrict goLazy listenerRef
                    cb <- mkEventCallback listenerRef
                    pure (cb, listenerRef)
                Just eventHdl -> pure eventHdl
        -- -- also get or make the ref callback
        -- refHdl <-
        --     case scn ^. _plan._elementals.at ri.to (fromMaybe (Elemental Nothing mempty))._reactListeners.at refN of
        --         Nothing -> do
        --             listenerRef <- newIORef mempty
        --             addEventListener exec (pure . JE.fromJSR) hdlRef listenerRef
        --             cb <- mkEventCallback listenerRef
        --             pure (cb, listenerRef)
        --         Just refHdl -> pure refHdl
        -- prepare the updated state
        let scn' = scn & _plan._elementals.at ri %~ (Just . addElem . initElem)
            -- scn' = scn & _plan._elementals.at ri %~ (Just . addElem . addRef . initElem)
            -- addRef = _reactListeners.at refN %~ addRefListener
            -- addRefListener = Just . maybe refHdl (const refHdl)
            initElem = fromMaybe (Elemental Nothing mempty)
            addElem = _reactListeners.at n %~ addListener
            addListener = Just . maybe eventHdl (const eventHdl)
        -- Update the subject
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
  where
    -- refN = J.pack "ref"
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj
    -- hdlRef x = do
    --     sbj <- view _subject
    --     postCmd' . doTickScene sbj $ (_plan._elementals.ix ri._elementalRef .= x)

addEventListener :: (NFData a)
    => (cmd -> IO ())
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> IORef (J.JSVal -> IO (), IO ())
    -> IO ()
addEventListener execIO goStrict goLazy listenerRef = do
    -- update the ioref with the new handler
    (preprocessor, postprocessor) <- mkEventHandler (goStrict . JE.toJSR)
    let postprocessor' = (`evalMaybeT` ()) $ do
            c <- goLazy <$> postprocessor
            lift $ execIO c
    atomicModifyIORef' listenerRef $ \hdl -> (hdl `mappendListener` (preprocessor, postprocessor'), ())

mappendListener :: (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ())
mappendListener (f1, g1) (f2, g2) = (\x -> f1 x *> f2 x, g1 *> g2)

execRegisterTickedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> Subject s
    -> cmd
    -> m ()
execRegisterTickedListener exec sbj c = do
    UnliftIO u <- askUnliftIO
    let hdl = u $ exec c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._tickedListener %~ (\(v, a) -> (v, a *> hdl))
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj

execRegisterMountedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> Subject s
    -> cmd
    -> m ()
execRegisterMountedListener exec sbj c = do
    UnliftIO u <- askUnliftIO
    let hdl = u $ exec c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._mountedListener %~ (\(v, a) -> (v, a *> hdl))
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj

execRegisterRenderedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> Subject s
    -> cmd
    -> m ()
execRegisterRenderedListener exec sbj c = do
    UnliftIO u <- askUnliftIO
    let hdl = u $ exec c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._renderedListener %~ (\(v, a) -> (v, a *> hdl))
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj

execRegisterNextRenderedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> Subject s
    -> cmd
    -> m ()
execRegisterNextRenderedListener exec sbj c = do
    UnliftIO u <- askUnliftIO
    let hdl = u $ exec c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._nextRenderedListener %~ (*> hdl)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj

execRegisterDOMListener ::
    ( NFData a
    , MonadUnliftIO m
    , Has (Tagged ReactId (MVar Int)) r
    , MonadReader r m
    )
    => (cmd -> m ())
    -> Subject s
    -> JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> m ()
execRegisterDOMListener exec sbj j n goStrict goLazy = do
    -- Add the handler to the state
    UnliftIO u <- askUnliftIO
    ri <- execMkReactId n
    liftIO $ do
        scn <- takeMVar scnVar
        -- since ri is unique, it'll always be a new map item
        -- update the ioref with the new handler
        listenerRef <- newIORef mempty
        addEventListener (u . exec) goStrict goLazy listenerRef
        cb <- mkEventCallback listenerRef
        -- prepare the updated state,
        let scn' = scn & _plan._domlListeners.at ri .~ (Just (cb, listenerRef))
                & _plan._finalCleanup %~ (*> removeDomListener j n cb)
        -- Update the subject
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        -- now add the domListener to the javascript target
        addDomListener j n cb
  where
    scnRef = sceneRef sbj
    scnVar = sceneVar sbj


addDomListener :: JE.JSRep -> J.JSString -> J.Callback (J.JSVal -> IO ()) -> IO ()
addDomListener j n cb = js_addDomListener (JE.toJS j) n (JE.toJS cb)

removeDomListener :: JE.JSRep -> J.JSString -> J.Callback (J.JSVal -> IO ()) -> IO ()
removeDomListener j n cb = js_removeDomListener (JE.toJS j) n (JE.toJS cb)

#ifdef __GHCJS__

foreign import javascript unsafe
    "console.log('addEventListener:' + $1 + ':' + $2); window.wocky = $1; if ($1 && $1['addEventListener']) { $1['addEventListener']($2, $3); }"
    js_addDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()

foreign import javascript unsafe
    "console.log('removeEventListener:' + $1 + ':' + $2); window.wocky = $1; if ($1 && $1['removeEventListener']) { $1['removeEventListener']($2, $3); }"
    js_removeDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()

#else

js_addDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()
js_addDomListener _ _ _ = pure mempty

js_removeDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()
js_removeDomListener _ _ _ = pure mempty

#endif
