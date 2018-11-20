{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor.Exec
    ( ReactorEnv(..)
    , mkReactorEnvIO
    , startApp
    , reactorBackgroundWork
    , execReactorCmd
    , execMkReactId
    , execSetRender
    , execMkObj
    , execKeepAliveObjUntilNextRender
    , execGetModel
    , execGetElementalRef
    , execRerender
    , execTickModel
    , execRegisterDOMListener
    , execRegisterReactListener
    , execRegisterMountedListener
    , execRegisterRenderedListener
    , execRegisterNextRenderedListener
    , execRegisterTickedListener
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Delegate
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Cont
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
import Data.Typeable
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Foreign.Export as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Component
import Glazier.React.Entity
import Glazier.React.EventTarget
import Glazier.React.Gadget
import Glazier.React.Markup
import Glazier.React.ReactDOM
import Glazier.React.ReactId.Internal
import Glazier.React.Reactor
import Glazier.React.ReadIORef
import Glazier.React.Model
import Glazier.React.Obj
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE
import System.Mem.Weak

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

data ReactorEnv = ReactorEnv
    { reactIdEnv :: MVar Int
    , reactorBackgroundEnv :: TQueue (IO (IO ()))
    }

makeLenses_ ''ReactorEnv

mkReactorEnvIO :: IO (ReactorEnv)
mkReactorEnvIO = ReactorEnv <$> (newMVar (0 :: Int)) <*> newTQueueIO

-- | An example of starting an app using the glazier-react framework
startApp ::
    ( MonadIO m
    , MonadReader r m
    , Has ReactorEnv r
    , Typeable s -- for J.export
    , AsReactor cmd
    , AsFacet (IO cmd) cmd
    )
    => (cmd -> m ()) -> Widget cmd s s () -> s -> JE.JSRep -> m ()
startApp executor wid s root = do
    -- background worker thread
    q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
    liftIO $ void $ forkIO $ forever $ reactorBackgroundWork q

    -- create a mvar to store the app subject
    objVar <- liftIO $ newEmptyMVar
    void $ liftIO $ mkWeakMVar objVar $ do
        putStrLn "LOUISDEBUG: App done"
    let setup = do
            obj <- mkObj' wid s
            exec' (command_ <$> (putMVar objVar obj))
        cs = (`execState` mempty) $ evalContT setup

    -- run the initial commands, this will store the app Obj into objVar
    traverse_ executor cs

    -- Start the App render
    liftIO $ do
        obj <- takeMVar objVar
        markup <- unReadIORef $ (`execStateT` mempty) $ displayObj obj
        e <- toElement markup
        renderDOM e root

        -- Export obj to prevent it from being garbage collected
        void $ J.export obj

-- LOUISFIXME: Simply
reactorBackgroundWork :: TQueue (IO (IO ())) -> IO ()
reactorBackgroundWork q = do
    -- wait until there is data
    x <- atomically $ readTQueue q
    -- run the action - this might add more data into the queue
    y <- x
    -- keep looping until there is no more data in the queue
    ys <- go (DL.singleton y)
    -- Run the secondary actions - which should rerender
    fold ys
  where
    go zs = do
        xs <- atomically $ flushTQueue q
        case xs of
            [] -> pure zs
            xs' -> do
                -- run the action - this might add more data into the queue
                ys <- sequence xs'
                go (zs <> DL.fromList ys)

execReactorCmd ::
    ( MonadUnliftIO m
    , MonadReader r m
    , AsReactor cmd
    , Has ReactorEnv r
    )
    => (cmd -> m ()) -> ReactorCmd cmd -> m ()
execReactorCmd executor c = case c of
    EvalIO n -> liftIO n >>= executor
    MkReactId n k -> execMkReactId n >>= (executor . k)
    SetRender obj w -> execSetRender obj w
    MkObj wid s k -> execMkObj executor wid s >>= (executor . k)
    MkObj2 wid s k -> execMkObj2 executor wid s >>= (executor . k)
    MkWeakObj s k -> execMkWeakObj s >>= (executor . k)
    -- GetModel obj k -> execGetModel obj >>= (executor . k)
    GetModel obj k -> void $ runMaybeT $ execGetModel obj >>= (lift . executor . k)
    GetElementalRef obj ri k -> execGetElementalRef executor obj ri k
    -- TickModel obj tick -> execTickModel obj tick >>= executor
    Rerender obj -> execRerender obj
    TickModel obj tick -> void $ runMaybeT $ execTickModel obj tick >>= (lift . executor)
    KeepAliveObjUntilNextRender obj s -> execKeepAliveObjUntilNextRender obj s
    RegisterDOMListener obj j n goStrict goLazy -> execRegisterDOMListener executor obj j n goStrict goLazy
    RegisterReactListener obj ri n goStrict goLazy -> execRegisterReactListener executor obj ri n goStrict goLazy
    RegisterMountedListener obj k -> execRegisterMountedListener executor obj k
    RegisterRenderedListener obj k -> execRegisterRenderedListener executor obj k
    RegisterNextRenderedListener obj k -> execRegisterNextRenderedListener executor obj k
    RegisterTickedListener obj k -> execRegisterTickedListener executor obj k

-----------------------------------------------------------------
execMkReactId ::
    ( MonadIO m
    , Has ReactorEnv r
    , MonadReader r m
    )
    => J.JSString
    -> m ReactId
execMkReactId n = do
    v <- view ((hasLens @ReactorEnv)._reactIdEnv)
    liftIO $ do
        i <- takeMVar v
        let i' = JE.safeIncrement i
        putMVar v i'
        pure . ReactId . J.append n . J.cons ':' . J.pack $ show i'

doRender :: Weak (IORef (Model s)) -> Window s () -> IO J.JSVal
doRender scnWkRef win = (`evalMaybeT` J.nullRef) $ do
    scnRef <- MaybeT $ deRefWeak scnWkRef
    lift $ do
        -- render using from scnRef (doesn't block)
        scn <- readIORef scnRef
        (mrkup, _) <- unReadIORef (execRWST win scn mempty) -- ignore unit writer output
        a <- JE.toJS <$> toElement mrkup
        pure a

doRef :: Weak (IORef (Model s)) -> Weak (MVar (Model s)) -> J.JSVal -> IO ()
doRef scnWkRef scnWkVar j = (`evalMaybeT` ()) $ do
    scnRef <- MaybeT $ deRefWeak scnWkRef
    scnVar <- MaybeT $ deRefWeak scnWkVar
    lift $ do
        -- update componentRef held in the Plan
        scn <- takeMVar scnVar
        let scn' = scn & _plan._componentRef .~ (JE.fromJS j)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

doRendered :: Weak (IORef (Model s)) -> Weak (MVar (Model s)) -> IO ()
doRendered scnWkRef scnWkVar = (`evalMaybeT` ()) $ do
    scnRef <- MaybeT $ deRefWeak scnWkRef
    scnVar <- MaybeT $ deRefWeak scnWkVar
    lift $ do
        -- update nextRenderedListener held in the Plan
        scn <- takeMVar scnVar
        let scn' = scn & _plan._nextRenderedListener .~ mempty
            nxt = scn ^. _plan._nextRenderedListener
            cb = scn ^. _plan._renderedListener

        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        nxt
        cb

doMounted :: Weak (IORef (Model s)) -> IO ()
doMounted scnWkRef = (`evalMaybeT` ()) $ do
    scnRef <- MaybeT $ deRefWeak scnWkRef
    lift $ do
        scn <- readIORef scnRef
        scn ^. _plan._mountedListener

execSetRender :: MonadIO m => WeakObj s -> Window s () -> m ()
execSetRender obj win = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execSetRender"
    scnRef <- MaybeT . liftIO . deRefWeak $ scnWkRef
    scnVar <- MaybeT . liftIO . deRefWeak $ scnWkVar
    -- create the callbacks
    liftIO $ do
        renderCb <- J.syncCallback' (doRender scnWkRef win)
        -- Replace the existing ShimCallbacks
        scn <- takeMVar scnVar
        -- replace the rendering function
        let origRenderCb = scn ^. _plan._shimCallbacks._shimRender
            scn' = scn & _plan._shimCallbacks._shimRender .~ renderCb
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        J.releaseCallback origRenderCb
  where
    scnWkRef = modelWeakRef obj
    scnWkVar = modelWeakVar obj

-- | Make an initialized 'Obj' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displayObj' should be used to render the subject.
execMkObj ::
    ( MonadIO m
    , AsReactor cmd
    , Has ReactorEnv r
    , MonadReader r m
    )
    => (cmd -> m ())
    -> Widget cmd s s ()
    -> s
    -> m (Obj s)
execMkObj executor wid s = do
    ri <- execMkReactId (J.pack "plan")
    (obj, cs) <- liftIO $ do
        -- create shim with fake callbacks for now
        let newPlan = Plan
                ri
                Nothing
                (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                False
                False
            scn = Model newPlan s

        scnRef <- newIORef scn
        scnVar <- newEmptyMVar

        scnWkRef <- mkWeakIORef scnRef $
            putStrLn "LOUISDEBUG: release scnRef"

        -- Create automatic garbage collection of the callbacks
        -- that will run when the Obj lease members are garbage collected.
        scnWkVar <- mkWeakMVar scnVar $ do
            putStrLn "LOUISDEBUG: release scnVar"
            scn' <- readIORef scnRef
            -- scn' ^. _plan._nextRenderedListener
            scn' ^. _plan._finalCleanup
            let cbs = scn' ^. _plan._shimCallbacks
            releaseShimCallbacks cbs
            -- cleanup callbacks
            traverse_ (traverse (J.releaseCallback . fst) . reactListeners) (scn' ^. _plan._elementals)
            traverse_ (J.releaseCallback . fst) (scn' ^. _plan._domlListeners)

        -- Create callbacks for now
        renderCb <- J.syncCallback' (pure J.nullRef) -- dummy render for now
        refCb <- J.syncCallback1 J.ContinueAsync (doRef scnWkRef scnWkVar)
        mountedCb <- J.syncCallback J.ContinueAsync (doMounted scnWkRef)
        renderedCb <- J.syncCallback J.ContinueAsync (doRendered scnWkRef scnWkVar)

        -- Now we have enough to make a subject
        let gad = runExceptT wid
            gad' = gad `bindLeft` setRndr
            gad'' = (either id id) <$> gad'
            -- This must be the only place obj is reference otherwise
            -- we will get memory leaks!
            tick = runGadget gad'' (Entity (WeakObj scnWkRef scnWkVar) id) pure
            cs = execState tick mempty
            -- update the model to include the real shimcallbacks
            scn' = scn & _plan._shimCallbacks .~ ShimCallbacks renderCb mountedCb renderedCb refCb
        -- update the mutable variables with the initialzed model
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        pure (Obj scnRef scnVar, cs)
    -- execute additional commands
    -- one of these commands will be 'SetRender' which will
    -- update the dummy render with the real render function.
    executor (command' $ DL.toList cs)
    -- return the subject
    pure obj
  where
    -- Use the obj from the env to avoid keeping obj alive
    setRndr win = do
        obj <- view _weakObj
        exec' $ SetRender obj win


-- | Make an initialized 'Obj' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displayObj' should be used to render the subject.
execMkObj2 ::
    ( MonadIO m
    , AsReactor cmd
    , Has ReactorEnv r
    , MonadReader r m
    )
    => (cmd -> m ())
    -> Widget cmd s s ()
    -> s
    -> m (Obj s)
execMkObj2 executor wid s = do
    ri <- execMkReactId (J.pack "plan")
    (obj, cs) <- liftIO $ do
        -- create shim with fake callbacks for now
        let newPlan = Plan
                ri
                Nothing
                (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                False
                False
            scn = Model newPlan s

        scnRef <- newIORef scn
        scnVar <- newEmptyMVar

        scnWkRef <- mkWeakIORef scnRef $
            putStrLn "LOUISDEBUG: release scnRef"

        -- Create automatic garbage collection of the callbacks
        -- that will run when the Obj lease members are garbage collected.
        scnWkVar <- mkWeakMVar scnVar $ do
            putStrLn "LOUISDEBUG: release scnVar"
            scn' <- readIORef scnRef
            -- scn' ^. _plan._nextRenderedListener
            scn' ^. _plan._finalCleanup
            let cbs = scn' ^. _plan._shimCallbacks
            releaseShimCallbacks cbs
            -- cleanup callbacks
            traverse_ (traverse (J.releaseCallback . fst) . reactListeners) (scn' ^. _plan._elementals)
            traverse_ (J.releaseCallback . fst) (scn' ^. _plan._domlListeners)

        -- Create callbacks for now
        renderCb <- J.syncCallback' (pure J.nullRef) -- dummy render for now
        refCb <- J.syncCallback1 J.ContinueAsync (doRef scnWkRef scnWkVar)
        mountedCb <- J.syncCallback J.ContinueAsync (doMounted scnWkRef)
        renderedCb <- J.syncCallback J.ContinueAsync (doRendered scnWkRef scnWkVar)

        -- Now we have enough to make a subject
        let gad = runExceptT wid
            gad' = gad `bindLeft` setRndr
            gad'' = (either id id) <$> gad'
            -- This must be the only place obj is reference otherwise
            -- we will get memory leaks!
            tick = runGadget gad'' (Entity (WeakObj scnWkRef scnWkVar) id) pure
            cs = execState tick mempty
            -- update the model to include the real shimcallbacks
            scn' = scn & _plan._shimCallbacks .~ ShimCallbacks renderCb mountedCb renderedCb refCb
        -- update the mutable variables with the initialzed model
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        pure (Obj scnRef scnVar, cs)
    -- execute additional commands
    -- one of these commands will be 'SetRender' which will
    -- update the dummy render with the real render function.
    executor (command' $ DL.toList cs)
    -- return the subject
    pure obj
  where
    -- Use the obj from the env to avoid keeping obj alive
    setRndr win = pure ()

execMkWeakObj ::
    ( MonadIO m
    )
    => Obj s
    -> m (WeakObj s)
execMkWeakObj (Obj scnRef scnVar) = liftIO $ do
    scnWkRef <- mkWeakIORef scnRef (pure ())
    scnWkVar <- mkWeakMVar scnVar (pure ())
    pure $ WeakObj scnWkRef scnWkVar

execKeepAliveObjUntilNextRender ::
    ( MonadIO m
    , MonadReader r m
    , Has ReactorEnv r
    )
    => WeakObj s -> Obj a -> m ()
execKeepAliveObjUntilNextRender obj s = void $ runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    liftIO $ do
        putStrLn "keeping alive"
        scn <- takeMVar scnVar
        let keepalive = prolong s *> putStrLn "touch"
            scn' = scn & _plan._nextRenderedListener %~ (*> keepalive)
        -- Update the back buffer
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
    -- trigger a rerender
    execRerender obj

execGetModel ::
    MonadIO m
    => WeakObj s
    -> MaybeT m s
execGetModel obj = do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    liftIO $ model <$> readIORef scnRef

execRerender ::
    ( MonadIO m
    , MonadReader r m
    , Has ReactorEnv r
    )
    => WeakObj s -> m ()
execRerender obj = void $ runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
    liftIO $ do
        scn <- takeMVar scnVar
        if not (scn ^. _plan._rerenderRequired)
            then do
                let scn' = scn & _plan._rerenderRequired .~ True
                -- Update the back buffer
                atomicWriteIORef scnRef scn'
                putMVar scnVar scn'
                atomically $ writeTQueue q (pure (scheduleRerender (Obj scnRef scnVar)))
            -- rerender has already been scheduled
            else putMVar scnVar scn

scheduleRerender :: Obj s -> IO ()
scheduleRerender obj = do
    scn <- takeMVar scnVar
    if scn ^. _plan._rerenderRequired
        then do
            let scn' = scn & _plan._rerenderRequired .~ False
                        & _plan._tickedNotified .~ False
            -- Update the back buffer
            atomicWriteIORef scnRef scn'
            putMVar scnVar scn'
            case scn ^. _plan._componentRef of
                Nothing -> pure ()
                Just j -> rerenderShim j
        -- rerender not required (eg. already processed)
        else putMVar scnVar scn
  where
    scnRef = modelRef obj
    scnVar = modelVar obj

-- | No need to run in a separate thread because it should never block for a significant amount of time.
-- Update the model 'MVar' with the given action. Also triggers a rerender.
execTickModel ::
    ( MonadIO m
    , MonadReader r m
    , Has ReactorEnv r
    )
    => WeakObj s
    -> ModelState s cmd
    -> MaybeT m cmd
execTickModel obj tick = do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
    liftIO $ do
        scn <- takeMVar scnVar
        let s = scn ^. _model
        (c, s') <- unReadIORef $ runStateT tick s
        let scn' = scn & _model .~ s'
        -- Update the back buffer
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        atomically $ writeTQueue q (notifyTicked (Obj scnRef scnVar))
        pure c
  where
    notifyTicked (Obj scnRef scnVar) = do
        scn <- takeMVar scnVar
        if not (scn ^. _plan._tickedNotified)
            then do
                -- LOUISFIXME: Document tickNotified/renderRequired lifecycle
                let scn' = scn & _plan._tickedNotified .~ True
                        & _plan._rerenderRequired .~ True
                    cb = scn ^. _plan._tickedListener
                -- Update the back buffer
                atomicWriteIORef scnRef scn'
                putMVar scnVar scn'
                -- run tickedListeener
                cb
                pure (scheduleRerender (Obj scnRef scnVar))
            -- notify not required (eg. already processed)
            else do
                putMVar scnVar scn
                pure (pure ())

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
    c <- newTQueueIO
    let preprocess evt = (`evalMaybeT` ()) $ do
            r <- goStrict evt
            -- This is guaranteed never to block
            lift $ atomically $ writeTQueue c $!! r
        -- there might not be a value in the chan
        -- because the preprocessor might not have produced any values
        postprocess = MaybeT $ atomically $ tryReadTQueue c
    pure (preprocess, postprocess)

addEventHandler :: (NFData a)
    => (JE.JSRep -> MaybeT IO a)
    -> (a -> IO ())
    -> IORef (J.JSVal -> IO (), IO ())
    -> IO ()
addEventHandler goStrict goLazy listenerRef = do
    -- update the ioref with the new handler
    (preprocessor, postprocessor) <- mkEventHandler (goStrict . JE.toJSR)
    let postprocessor' = (`evalMaybeT` ()) (postprocessor >>= (lift . goLazy))
    atomicModifyIORef' listenerRef $ \hdl -> (hdl `mappendListener` (preprocessor, postprocessor'), ())

-- | Create ref handler to assign 'elementalRef'
data Freshness = Existing | Fresh

execGetElementalRef ::
    ( MonadUnliftIO m
    , MonadReader r m
    , Has ReactorEnv r
    )
    => (cmd -> m ())
    -> WeakObj s
    -> ReactId
    -> (EventTarget -> cmd)
    -> m ()
execGetElementalRef executor obj ri k = void . runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    UnliftIO u <- lift askUnliftIO
    scn <- liftIO $ takeMVar scnVar
    (refFreshness, pln) <- lift $ getOrRegisterRefCoreListener obj ri (plan scn)
    let tryAgain = u $ execGetElementalRef executor obj ri k
        pln' = pln & _nextRenderedListener %~ (*> tryAgain)
        scn' = scn & _plan .~ pln'
        ret = pln ^? (_elementals.ix ri._elementalRef._Just)
        doTryAgain = do
            liftIO $ do
                atomicWriteIORef scnRef scn'
                putMVar scnVar scn'
            -- trigger a rerender
            execRerender obj
    case refFreshness of
        Fresh -> doTryAgain
        Existing -> case ret of
            Nothing -> doTryAgain
            Just ret' -> do
                liftIO $ putMVar scnVar scn
                lift . executor . k $ ret'

getOrRegisterRefCoreListener :: (MonadIO m)
    => WeakObj s
    -> ReactId
    -> Plan
    -> m (Freshness, Plan)
getOrRegisterRefCoreListener obj ri pln = do
    liftIO $ do
        -- first get or make the target
        (freshness, eventHdl) <-
            case pln ^. _elementals.at ri.to (fromMaybe (Elemental Nothing mempty))._reactListeners.at n of
                Nothing -> do
                    listenerRef <- newIORef mempty
                    cb <- mkEventCallback listenerRef
                    -- update listenerRef with new event listener
                    -- only do this once (when for first ref listener)
                    addEventHandler (pure . JE.fromJSR) hdlRef listenerRef
                    pure (Fresh, (cb, listenerRef))
                Just eventHdl -> pure (Existing, eventHdl)
        -- prepare the updated state
        let pln' = pln & _elementals.at ri %~ (Just . addElem . initElem)
            initElem = fromMaybe (Elemental Nothing mempty)
            addElem = _reactListeners.at n %~ addListener
            addListener = Just . maybe eventHdl (const eventHdl)
        case freshness of
            Fresh -> pure (Fresh, pln')
            Existing -> pure (Existing, pln)
  where
    n = J.pack "ref"
    -- hdlRef x = command' $ TickModel obj (command_ <$> (_plan._elementals.ix ri._elementalRef .= x))
    hdlRef x = void . runMaybeT $ do
        scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
        scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
        lift $ do
            scn <- takeMVar scnVar
            let scn' = scn & _plan._elementals.ix ri._elementalRef .~ x
            -- Update the back buffer
            atomicWriteIORef scnRef scn'
            putMVar scnVar scn'

execRegisterReactListener :: (NFData a, MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> m ()
execRegisterReactListener executor obj ri n goStrict goLazy = void . runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    UnliftIO u <- lift askUnliftIO
    scn_ <- liftIO $ takeMVar scnVar
    -- special logic for ref, where the first ref handler must be 'registerRefCoreListener'
    scn <- if (n == (J.pack "ref"))
        then do
            (refFreshness, pln) <- getOrRegisterRefCoreListener obj ri (plan scn_)
            case refFreshness of
                Existing -> pure scn_
                Fresh -> pure (scn_ & _plan .~ pln)
        else pure scn_
    liftIO $ do
        -- get or make the target
        (freshness, eventHdl@(_, listenerRef)) <-
            case scn ^. _plan._elementals.at ri.to (fromMaybe (Elemental Nothing mempty))._reactListeners.at n of
                Nothing -> do
                    listenerRef <- newIORef mempty
                    cb <- mkEventCallback listenerRef
                    pure (Fresh, (cb, listenerRef))
                Just eventHdl -> pure (Existing, eventHdl)
        let scn' = case freshness of
                Fresh -> scn & _plan._elementals.at ri %~ (Just . addElem . initElem)
                Existing -> scn
            initElem = fromMaybe (Elemental Nothing mempty)
            addElem = _reactListeners.at n %~ addListener
            addListener = Just . maybe eventHdl (const eventHdl)
        -- update listenerRef with new event listener
        addEventHandler goStrict (u . executor . goLazy) listenerRef
        -- Update the subject
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

mappendListener :: (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ())
mappendListener (f1, g1) (f2, g2) = (\x -> f1 x *> f2 x, g1 *> g2)

execRegisterTickedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> cmd
    -> m ()
execRegisterTickedListener executor obj c = void . runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._tickedListener %~ (*> hdl)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

execRegisterMountedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> cmd
    -> m ()
execRegisterMountedListener executor obj c = void . runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._mountedListener %~ (*> hdl)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

execRegisterRenderedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> cmd
    -> m ()
execRegisterRenderedListener executor obj c = void . runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._renderedListener %~ (*> hdl)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

execRegisterNextRenderedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> cmd
    -> m ()
execRegisterNextRenderedListener executor obj c = void . runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._nextRenderedListener %~ (*> hdl)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

execRegisterDOMListener ::
    ( NFData a
    , MonadUnliftIO m
    , Has ReactorEnv r
    , MonadReader r m
    )
    => (cmd -> m ())
    -> WeakObj s
    -> JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> m ()
execRegisterDOMListener executor obj j n goStrict goLazy = void . runMaybeT $ do
    scnRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    scnVar <- MaybeT . liftIO . deRefWeak $ modelWeakVar obj
    -- Add the handler to the state
    UnliftIO u <- lift askUnliftIO
    -- generate a unique id
    ri <- execMkReactId n
    liftIO $ do
        scn <- takeMVar scnVar
        -- since ri is unique, it'll always be a new map item
        -- update the ioref with the new handler
        listenerRef <- newIORef mempty
        cb <- mkEventCallback listenerRef
        addEventHandler goStrict (u . executor . goLazy) listenerRef
        -- prepare the updated state,
        let scn' = scn & _plan._domlListeners.at ri .~ (Just (cb, listenerRef))
                & _plan._finalCleanup %~ (*> removeDomListener j n cb)
        -- Update the subject
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        -- now add the domListener to the javascript target
        addDomListener j n cb

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
