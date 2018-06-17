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
import Data.IORef
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tagged
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Component
import Glazier.React.Entity
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
    RegisterReactListener sbj ri n goStrict goLazy -> execRegisterReactListener exec sbj ri n goStrict goLazy
    RegisterRenderedListener sbj c' -> execRegisterRenderedListener exec sbj c'
    RegisterDOMListener sbj j n goStrict goLazy -> execRegisterDOMListener exec sbj j n goStrict goLazy

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
    (mrkup, _) <- unReadIORef (execRWST win scn mempty) -- ignore unit writer output
    JE.toJS <$> toElement mrkup

doRef :: IORef (Scene s) -> MVar (Scene s) -> J.JSVal -> IO ()
doRef scnRef scnVar j = do
    -- update componentRef held in the Plan
    scn <- takeMVar scnVar
    let scn' = scn & _plan._componentRef .~ JE.fromJS j
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'

doOnRendered :: IORef (Scene s) -> MVar (Scene s) -> IO ()
doOnRendered scnRef scnVar = do
    -- update tmpCleanup held in the Plan
    scn <- takeMVar scnVar
    let scn' = scn & _plan._tmpCleanup .~ mempty
        cln = scn ^. _plan._tmpCleanup
    atomicWriteIORef scnRef scn'
    putMVar scnVar scn'
    cln
    scn ^. _plan._renderedListener

execSetRender :: MonadIO m => Subject s -> Window s () -> m ()
execSetRender (Subject scnRef scnVar renderLeaseRef _) win = liftIO $ do
    -- create the callbacks
    renderCb <- J.syncCallback' (doRender scnRef win)
    -- Create automatic garbage collection of the callbacks
    -- that will run when the Subject lease members are garbage collected.
    renderLease <- liftIO $ newEmptyMVar
    void $ mkWeakMVar renderLease $ J.releaseCallback renderCb
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
    (sbj, cs) <- liftIO $ do
        -- create shim with fake callbacks for now
        let newPlan = Plan
                Nothing
                (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
                mempty
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
        otherCallbackLease <- newEmptyMVar
        renderLease <- newEmptyMVar
        renderLeaseRef <- newIORef renderLease
        -- Create callbacks for now
        renderCb <- J.syncCallback' (pure J.nullRef) -- dummy render for now
        refCb <- J.syncCallback1 J.ContinueAsync (doRef scnRef scnVar)
        renderedCb <- J.syncCallback J.ContinueAsync (doOnRendered scnRef scnVar)
        -- Create automatic garbage collection of the callbacks
        -- that will run when the Subject lease members are garbage collected.
        void $ mkWeakMVar otherCallbackLease $ do
            scn' <- readIORef scnRef
            scn' ^. _plan._tmpCleanup
            scn' ^. _plan._finalCleanup
            -- cleanup callbacks
            traverse (traverse (J.releaseCallback . fst) . reactListener) (scn' ^. _plan._elementals)
            J.releaseCallback refCb
            J.releaseCallback renderedCb
        void $ mkWeakMVar renderLease $ J.releaseCallback renderCb

        -- Now we have enough to make a subject
        let sbj = Subject scnRef scnVar renderLeaseRef otherCallbackLease
            -- initalize the subject using the Gadget
            gad = runExceptT wid
            gad' = gad `bindLeft` (postCmd' . SetRender sbj)
            gad'' = (either id id) <$> gad'
            tick = runGadget gad'' (Entity sbj id) pure
            cs = execState tick mempty
            -- update the scene to include the real shimcallbacks
            scn' = scn & _plan._shimCallbacks .~ ShimCallbacks renderCb refCb renderedCb
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
        eventHdl@(_, listenerRef) <-
            case scn ^. _plan._elementals.at ri.to (fromMaybe (Elemental Nothing mempty))._reactListener.at n of
                Nothing -> do
                    listenerRef <- newIORef mempty
                    cb <- mkEventCallback listenerRef
                    pure (cb, listenerRef)
                Just eventHdl -> pure eventHdl
        -- prepare the updated state
        let scn' = scn & _plan._elementals.at ri %~ addElem
            addElem = Just . maybe (Elemental Nothing (M.singleton n eventHdl))
                (_reactListener.at n %~ addListener)
            addListener = Just . maybe eventHdl (const eventHdl)
        -- update the ioref with the new handler
        (preprocessor, postprocessor) <- mkEventHandler (goStrict . JE.toJSR)
        let postprocessor' = (`evalMaybeT` ()) $ do
                c <- goLazy <$> postprocessor
                lift $ u $ exec c
        atomicModifyIORef' listenerRef $ \hdl -> (hdl <> (preprocessor, postprocessor'), ())
        -- Update the subject
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
    liftIO $ atomicModifyIORef' scnRef $ \scn ->
        (scn & _plan._renderedListener %~ (<> hdl), ())
  where
    scnRef = sceneRef sbj

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
        (preprocessor, postprocessor) <- mkEventHandler (goStrict . JE.toJSR)
        let postprocessor' = (`evalMaybeT` ()) $ do
                c <- goLazy <$> postprocessor
                lift $ u $ exec c
        listenerRef <- newIORef (preprocessor, postprocessor')
        cb <- mkEventCallback listenerRef
        -- prepare the updated state,
        let scn' = scn & _plan._domlListeners.at ri .~ (Just (cb, listenerRef))
                & _plan._finalCleanup %~ (<> removeDomListener j n cb)
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
    "if ($1 && $1['addEventListener']) { $1['addEventListener']($2, $3); }"
    js_addDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()

foreign import javascript unsafe
    "if ($1 && $1['removeEventListener']) { $1['removeEventListener']($2, $3); }"
    js_removeDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()

#else

js_addDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()
js_addDomListener _ _ _ = pure mempty

js_removeDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()
js_removeDomListener _ _ _ = pure mempty

#endif
