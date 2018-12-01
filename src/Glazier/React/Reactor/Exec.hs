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
    , execGetModel
    , execGetElementalRef
    , execRerender
    , execDoRerender
    , execMutate
    , execNotifyMutated
    , execResetMutation
    , execRegisterDOMListener
    , execRegisterReactListener
    , execRegisterMountedListener
    , execRegisterRenderedListener
    , execRegisterNextRenderedListener
    , execRegisterMutatedListener
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
import Glazier.React.Model
import Glazier.React.Obj.Internal
import Glazier.React.ReactDOM
import Glazier.React.ReactId.Internal
import Glazier.React.Reactor
import Glazier.React.ReadIORef
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE
import System.Mem.Weak

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

newtype ReactorEnv = ReactorEnv
    { reactIdVar :: MVar Int
    -- , reactorBackgroundEnv :: TQueue (IO (IO ()))
    }

makeLenses_ ''ReactorEnv

mkReactorEnvIO :: IO (ReactorEnv)
-- mkReactorEnvIO = ReactorEnv <$> (newMVar (0 :: Int)) <*> newTQueueIO
mkReactorEnvIO = ReactorEnv <$> (newMVar (0 :: Int))

-- | An example of starting an app using the glazier-react framework
startApp ::
    ( MonadIO m
    -- , MonadReader r m
    -- , Has ReactorEnv r
    , Typeable s -- for J.export
    , AsReactor cmd
    , AsFacet (IO cmd) cmd
    )
    => (cmd -> m ()) -> Widget cmd s s () -> s -> JE.JSRep -> m ()
startApp executor wid s root = do
    -- background worker thread
    -- q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
    -- liftIO $ void $ forkIO $ forever $ reactorBackgroundWork q

    -- create a mvar to store the app subject
    objVar <- liftIO $ newEmptyMVar
    let setup = do
            obj <- mkObj' wid s
            exec' (command_ <$> (putMVar objVar obj))
        cs = (`execState` mempty) $ runProgramT $ evalContT setup

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

-- | A runner of a queue of tasks that return another task to put into
-- the *back* of the queue after the queue is completely empty.
reactorBackgroundWork :: TQueue (IO (IO ())) -> IO ()
reactorBackgroundWork q = do
    -- wait until there is data
    void $ atomically $ peekTQueue q
    -- now consume the entire queue
    finalActions <- go mempty
    -- run saved actions that run after there is no more work in the queue
    fold finalActions
  where
    go zs = do
        xs <- atomically $ flushTQueue q
        case xs of
            [] -> pure zs
            xs' -> do
                -- run the tasks - this might add more data into the queue
                ys <- sequence xs'
                -- keep trying until queue is complete empty
                go (zs <> DL.fromList ys)

-- | Returns commands that need to be processed last
execReactorCmd ::
    ( MonadUnliftIO m
    , MonadReader r m
    , AsReactor cmd
    , Has ReactorEnv r
    )
    => (cmd -> m ()) -> ReactorCmd cmd -> m [cmd]
execReactorCmd executor c = case c of
    EvalIO n -> liftIO n >>= (done . executor)
    MkReactId n k -> execMkReactId n >>= (done . executor . k)
    SetRender obj w -> done $ execSetRender obj w
    MkObj wid s k -> execMkObj executor wid s >>= (done . executor . k)
    GetModel obj k -> (`evalMaybeT` []) $ execGetModel obj >>= (lift . done . executor . k)
    GetElementalRef obj k f -> done $ execGetElementalRef executor obj k f
    Rerender obj -> execRerender False obj
    DoRerender obj -> done $ execDoRerender obj
    Mutate cap k obj tick -> (`evalMaybeT` []) $ do
        liftIO $ putStrLn $ "Caption: " <> J.unpack cap
        (lastCmds, nextCmd) <- execMutate k obj tick
        lift $ executor nextCmd
        pure lastCmds
    NotifyMutated k obj -> execNotifyMutated k obj
    ResetMutation obj -> done $ execResetMutation obj
    RegisterDOMListener obj j n goStrict goLazy -> done $ execRegisterDOMListener executor obj j n goStrict goLazy
    RegisterReactListener obj k n goStrict goLazy -> done $ execRegisterReactListener executor obj k n goStrict goLazy
    RegisterMountedListener obj k -> done $ execRegisterMountedListener executor obj k
    RegisterRenderedListener obj k -> done $ execRegisterRenderedListener executor obj k
    RegisterNextRenderedListener obj k -> done $ execRegisterNextRenderedListener executor obj k
    RegisterMutatedListener obj k -> done $ execRegisterMutatedListener executor obj k
  where
    done f = (\() -> []) <$> f

-----------------------------------------------------------------
execMkReactId ::
    ( MonadIO m
    , Has ReactorEnv r
    , MonadReader r m
    )
    => J.JSString
    -> m ReactId
execMkReactId n = do
    v <- view ((hasLens @ReactorEnv)._reactIdVar)
    liftIO $ do
        i <- takeMVar v
        let i' = JE.safeIncrement i
        putMVar v i'
        pure $ ReactId n i'

__render :: WeakObj s -> Window s () -> IO J.JSVal
__render obj win = (`evalMaybeT` J.nullRef) $ do
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
    lift $ do
        -- render using from mdlRef (doesn't block)
        scn <- readIORef mdlRef
        (mrkup, _) <- unReadIORef (execRWST win scn mempty) -- ignore unit writer output
        a <- JE.toJS <$> toElement mrkup
        pure a

__ref :: WeakObj s -> J.JSVal -> IO ()
__ref obj j = (`evalMaybeT` ()) $ do
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    lift $ do
        -- update componentRef held in the Plan
        scn <- takeMVar mdlVar
        let scn' = scn & _plan._componentRef .~ (JE.fromJS j)
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'

__rendered :: WeakObj s -> IO ()
__rendered obj = (`evalMaybeT` ()) $ do
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    lift $ do
        -- update nextRenderedListener held in the Plan
        scn <- takeMVar mdlVar
        let scn' = scn & _plan._nextRenderedListener .~ mempty
            nxt = scn ^. _plan._nextRenderedListener
            cb = scn ^. _plan._renderedListener

        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'
        nxt
        cb

__mounted :: WeakObj s -> IO ()
__mounted obj = (`evalMaybeT` ()) $ do
    mdlRef <- MaybeT . deRefWeak $ modelWeakRef obj
    lift $ do
        scn <- readIORef mdlRef
        scn ^. _plan._mountedListener

execSetRender :: MonadIO m => WeakObj s -> Window s () -> m ()
execSetRender obj win = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execSetRender"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    -- create the callbacks
    liftIO $ do
        renderCb <- J.syncCallback' (__render obj win)
        -- Replace the existing ShimCallbacks
        scn <- takeMVar mdlVar
        -- replace the rendering function
        let origRenderCb = scn ^. _plan._shimCallbacks._shimRender
            scn' = scn & _plan._shimCallbacks._shimRender .~ renderCb
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'
        J.releaseCallback origRenderCb

-- | Make an initialized 'Obj' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displayObj' should be used to render the subject.
execMkObj ::
    ( MonadIO m
    , Has ReactorEnv r
    , MonadReader r m
    , AsReactor cmd
    )
    => (cmd -> m ())
    -> Widget cmd s s ()
    -> s
    -> m (Obj s)
execMkObj executor wid s = do
    liftIO $ putStrLn "LOUISDEBUG: execMkObj"
    k <- execMkReactId (J.pack "plan")
    (obj, cs) <- liftIO $ do
        -- create shim with fake callbacks for now
        let newPlan = Plan
                k
                Nothing
                (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                NotMutated
                RerenderNotRequired
            scn = Model newPlan s

        mdlRef <- newIORef scn
        mdlVar <- newEmptyMVar

        mdlWkRef <- mkWeakIORef mdlRef $
            putStrLn "LOUISDEBUG: release mdlRef"

        -- Create automatic garbage collection of the callbacks
        -- that will run when the Obj lease members are garbage collected.
        mdlWkVar <- mkWeakMVar mdlVar $ do
            putStrLn "LOUISDEBUG: release mdlVar"
            scn' <- readIORef mdlRef
            -- scn' ^. _plan._nextRenderedListener
            scn' ^. _plan._finalCleanup
            let cbs = scn' ^. _plan._shimCallbacks
            releaseShimCallbacks cbs
            -- cleanup callbacks
            traverse_ (traverse (J.releaseCallback . fst) . reactListeners) (scn' ^. _plan._elementals)
            traverse_ (J.releaseCallback . fst) (scn' ^. _plan._domlListeners)

        let obj = WeakObj mdlWkRef mdlWkVar
        -- Create callbacks for now
        renderCb <- J.syncCallback' (pure J.nullRef) -- dummy render for now
        refCb <- J.syncCallback1 J.ContinueAsync (__ref obj)
        mountedCb <- J.syncCallback J.ContinueAsync (__mounted obj)
        renderedCb <- J.syncCallback J.ContinueAsync (__rendered obj)

        -- Now we have enough to make a subject
        let gad = runExceptT wid
            gad' = gad `bindLeft` setRndr
            gad'' = (either id id) <$> gad'
            tick = runProgramT $ (`evalStateT` mempty) $ runGadget gad'' (Entity obj id) pure
            cs = execState tick mempty
            -- update the model to include the real shimcallbacks
            scn' = scn & _plan._shimCallbacks .~ ShimCallbacks renderCb mountedCb renderedCb refCb
        -- update the mutable variables with the initialzed model
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'
        pure (Obj obj mdlRef mdlVar, cs)
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

execGetModel ::
    MonadIO m
    => WeakObj s
    -> MaybeT m s
execGetModel obj = do
    liftIO $ putStrLn "LOUISDEBUG: execGetModel"
    mdlRef <- MaybeT . liftIO . deRefWeak $ modelWeakRef obj
    liftIO $ model <$> readIORef mdlRef

execRerender ::
    ( MonadIO m
    , AsReactor cmd
    )
    => Bool -> WeakObj s -> m [cmd]
execRerender rerenderSuppressed obj = (`evalMaybeT` []) $ do
    liftIO $ putStrLn "LOUISDEBUG: execRerender"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    liftIO $ do
        scn <- takeMVar mdlVar
        -- don't rerender straight away, but schedule a rerender
        -- when the background worker thread
        -- so multiple rerender requests will be naturally throttled
        if scn ^. _plan._rerendering == RerenderNotRequired
            || (rerenderSuppressed && scn ^. _plan._rerendering == RerenderSuppressed)
            then do
                let scn' = scn & _plan._rerendering .~ RerenderRequired
                -- Update the back buffer
                atomicWriteIORef mdlRef scn'
                putMVar mdlVar scn'
                -- Schedule a rerender
                pure [command' $ DoRerender obj]
            -- rerender has already been scheduled
            else do
                putMVar mdlVar scn
                pure []

execDoRerender :: MonadIO m => WeakObj s -> m ()
execDoRerender obj = (`evalMaybeT` ()) $ do
    liftIO $ putStrLn "LOUISDEBUG: execDoRerender"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    liftIO $ do
        scn <- takeMVar mdlVar
        if scn ^. _plan._rerendering == RerenderRequired
            then do
                let scn' = scn & _plan._rerendering .~ RerenderNotRequired
                -- Update the back buffer
                atomicWriteIORef mdlRef scn'
                putMVar mdlVar scn'
                case scn ^. _plan._componentRef of
                    Nothing -> pure ()
                    Just j -> rerenderShim j
            -- rerender not required (eg. already processed)
            else putMVar mdlVar scn

-- | No need to run in a separate thread because it should never block for a significant amount of time.
-- Update the model 'MVar' with the given action. Also triggers a rerender.
-- Returns (cmds to process last, cmds to process first)
execMutate ::
    ( MonadIO m
    , AsReactor cmd
    )
    => ReactId
    -> WeakObj s
    -> ModelState s cmd
    -> MaybeT m ([cmd], cmd)
execMutate k obj tick = do
    liftIO $ putStrLn "LOUISDEBUG: execMutate"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    -- q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
    liftIO $ do
        scn <- takeMVar mdlVar
        let s = scn ^. _model
        (c, s') <- unReadIORef $ runStateT tick s
        let scn' = scn & _model .~ s'
        -- don't notify ticked listener straight away, but schedule it like rerender
        -- This is so that if there are multiple mutates, it will try to only fire
        -- the notified callback as late as possible.
        let mutation' = scn' ^. _plan._mutation
            -- Suppress rerender until after mutatedListener is called
            scn'' = scn' & _plan._rerendering .~ RerenderSuppressed
            notifyCmd = [command' $ NotifyMutated k obj]
            (c', scn''') = case mutation' of
                NotMutated -> (notifyCmd, scn'' & _plan._mutation .~ Mutated)
                _ -> ([], scn'')
        -- Update the back buffer
        atomicWriteIORef mdlRef scn'''
        putMVar mdlVar scn'''
        pure (c', c)

-- LOUISFIXME: Document tickNotified/renderRequired lifecycle
-- ensuring that the mutatedListener callback is called.
-- at most once per reactorBackgroundBatch
-- Want to catch the case if two obj ticked and listen to each other
-- then we won't get infinite work
-- to also schedule a rerender
-- Returns commands to process last
execNotifyMutated ::
    ( MonadIO m
    , AsReactor cmd
    )
    => ReactId -> WeakObj s -> m [cmd]
execNotifyMutated k obj = (`evalMaybeT` []) $ do
    liftIO $ putStrLn "LOUISDEBUG: execNotifyMutated"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    liftIO $ do
        scn <- takeMVar mdlVar
        if scn ^. _plan._mutation == Mutated
            then do
                -- Don't reset back to NotMutated to avoid
                -- infinite loops if the mutatedListener mutates this same object
                let scn' = scn & _plan._mutation .~ MutationNotified
                    cb = scn ^. _plan._mutatedListener
                -- Update the back buffer
                atomicWriteIORef mdlRef scn'
                putMVar mdlVar scn'
                -- run mutatedListener
                cb k
                -- force shedule a rerender
                cs <- execRerender True obj
                -- schedule reset mutation and rerendering
                pure $ (command' $ ResetMutation obj) : cs
            -- notify not required (eg. already processed)
            else do
                putMVar mdlVar scn
                pure []

-- LOUISFIXME: Document tickNotified/renderRequired lifecycle
execResetMutation :: MonadIO m => WeakObj s -> m ()
execResetMutation obj = (`evalMaybeT` ()) $ do
    liftIO $ putStrLn "LOUISDEBUG: execResetMutation"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    liftIO $ do
        scn <- takeMVar mdlVar
        if scn ^. _plan._mutation == MutationNotified
            then do
                let scn' = scn & _plan._mutation .~ NotMutated
                -- Update the back buffer
                atomicWriteIORef mdlRef scn'
                putMVar mdlVar scn'
            else putMVar mdlVar scn

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
-- by allowing a way for all the preprocessor handlers to run first before
-- running all of the postprocessor handlers.
mkEventHandler :: (NFData a) => (evt -> MaybeT IO a) -> IO (evt -> IO (), MaybeT IO a)
mkEventHandler goStrict = do
    liftIO $ putStrLn "LOUISDEBUG: mkEventHandler"
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
    )
    => (cmd -> m ())
    -> WeakObj s
    -> ReactId
    -> (EventTarget -> cmd)
    -> m ()
execGetElementalRef executor obj k f = (`evalMaybeT` ()) $ do
    liftIO $ putStrLn "LOUISDEBUG: execGetElementalRef"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    UnliftIO u <- lift askUnliftIO
    scn <- liftIO $ takeMVar mdlVar
    (refFreshness, pln) <- lift $ getOrRegisterRefCoreListener obj k (plan scn)
    let tryAgain = u $ execGetElementalRef executor obj k f
        scn' = scn & _plan .~ pln -- to save the core listener
                & _plan._nextRenderedListener %~ (*> tryAgain)
        ret = pln ^? (_elementals.ix k._elementalRef._Just)
        triggerTryAgain = liftIO $ do
            liftIO $ putStrLn "LOUISDEBUG: triggerTryAgain"
            -- save the tryAgain handler when nextRendered
            atomicWriteIORef mdlRef scn'
            putMVar mdlVar scn'
            -- trigger a rerender immediately
            -- Note: componentRef should be set
            -- since it is part of the shimComponent wrapper
            -- which should have been initialized by now.
            -- In the very unlikely case it is not set
            -- (eg still initializing?) then there is nothing
            -- we can do for now.
            -- But as soon as the initialzation finishes, and renders
            -- the tryAgain callback will be called.
            case scn' ^. _plan._componentRef of
                Nothing -> pure ()
                Just j -> rerenderShim j
    case refFreshness of
        Fresh -> do
            liftIO $ putStrLn "LOUISDEBUG: Fresh"
            triggerTryAgain
        Existing -> case ret of
            Nothing -> do
                liftIO $ putStrLn "LOUISDEBUG: Existing Nothing"
                triggerTryAgain
            Just ret' -> do
                liftIO $ putStrLn "LOUISDEBUG: Existing Just"
                liftIO $ putMVar mdlVar scn
                lift . executor . f $ ret'

getOrRegisterRefCoreListener :: (MonadIO m)
    => WeakObj s
    -> ReactId
    -> Plan
    -> m (Freshness, Plan)
getOrRegisterRefCoreListener obj k pln = do
    liftIO $ putStrLn "LOUISDEBUG: getOrRegisterRefCoreListener"
    liftIO $ do
        -- first get or make the target
        (freshness, eventHdl) <-
            case pln ^. _elementals.at k.to (fromMaybe (Elemental Nothing mempty))._reactListeners.at n of
                Nothing -> do
                    liftIO $ putStrLn $ "LOUISDEBUG: RefCore Nothing " <> J.unpack (fullReactId k)
                    listenerRef <- newIORef mempty
                    cb <- mkEventCallback listenerRef
                    -- update listenerRef with new event listener
                    -- only do this once (when for first ref listener)
                    addEventHandler (pure . JE.fromJSR) hdlRef listenerRef
                    pure (Fresh, (cb, listenerRef))
                Just eventHdl -> do
                    liftIO $ putStrLn $ "LOUISDEBUG: RefCore Just " <> J.unpack (fullReactId k)
                    pure (Existing, eventHdl)
        -- prepare the updated state
        let pln' = pln & _elementals.at k %~ (Just . addElem . initElem)
            initElem = fromMaybe (Elemental Nothing mempty)
            addElem = _reactListeners.at n %~ addListener
            addListener = Just . maybe eventHdl (const eventHdl)
        case freshness of
            Fresh -> pure (Fresh, pln')
            Existing -> pure (Existing, pln)
  where
    n = J.pack "ref"
    -- hdlRef x = command' $ Mutate obj (command_ <$> (_plan._elementals.ix k._elementalRef .= x))
    hdlRef x = void . runMaybeT $ do
        obj' <- deRefWeakObj obj
        let mdlRef = modelRef obj'
            mdlVar = modelVar obj'
        lift $ do
            scn <- takeMVar mdlVar
            let scn' = scn & _plan._elementals.ix k._elementalRef .~ x
            -- Update the back buffer
            atomicWriteIORef mdlRef scn'
            putMVar mdlVar scn'

execRegisterReactListener :: (NFData a, MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> cmd)
    -> m ()
execRegisterReactListener executor obj k n goStrict goLazy = void . runMaybeT $ do
    liftIO $ putStrLn $ "LOUISDEBUG: execRegisterReactListener " <> J.unpack (fullReactId k)
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    UnliftIO u <- lift askUnliftIO
    scn_ <- liftIO $ takeMVar mdlVar
    -- special logic for ref, where the first ref handler must be 'registerRefCoreListener'
    scn <- if (n == (J.pack "ref"))
        then do
            (refFreshness, pln) <- getOrRegisterRefCoreListener obj k (plan scn_)
            case refFreshness of
                Existing -> pure scn_
                Fresh -> pure (scn_ & _plan .~ pln)
        else pure scn_
    liftIO $ do
        -- get or make the target
        (freshness, eventHdl@(_, listenerRef)) <-
            case scn ^. _plan._elementals.at k.to (fromMaybe (Elemental Nothing mempty))._reactListeners.at n of
                Nothing -> do
                    listenerRef <- newIORef mempty
                    cb <- mkEventCallback listenerRef
                    pure (Fresh, (cb, listenerRef))
                Just eventHdl -> pure (Existing, eventHdl)
        let scn' = case freshness of
                Fresh -> scn & _plan._elementals.at k %~ (Just . addElem . initElem)
                Existing -> scn
            initElem = fromMaybe (Elemental Nothing mempty)
            addElem = _reactListeners.at n %~ addListener
            addListener = Just . maybe eventHdl (const eventHdl)
        -- update listenerRef with new event listener
        addEventHandler goStrict (u . executor . goLazy) listenerRef
        -- Update the subject
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'

mappendListener :: (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ())
mappendListener (f1, g1) (f2, g2) = (\x -> f1 x *> f2 x, g1 *> g2)

execRegisterMutatedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> (ReactId -> cmd)
    -> m ()
execRegisterMutatedListener executor obj f = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterMutatedListener"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u . executor . f
    liftIO $ do
        scn <- takeMVar mdlVar
        let scn' = scn & _plan._mutatedListener %~ (\g -> \k -> g k *> hdl k)
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'

execRegisterMountedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> cmd
    -> m ()
execRegisterMountedListener executor obj c = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterMountedListener"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar mdlVar
        let scn' = scn & _plan._mountedListener %~ (*> hdl)
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'

execRegisterRenderedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> cmd
    -> m ()
execRegisterRenderedListener executor obj c = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterRenderedListener"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar mdlVar
        let scn' = scn & _plan._renderedListener %~ (*> hdl)
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'

execRegisterNextRenderedListener :: (MonadUnliftIO m)
    => (cmd -> m ())
    -> WeakObj s
    -> cmd
    -> m ()
execRegisterNextRenderedListener executor obj c = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterNextRenderedListener"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar mdlVar
        let scn' = scn & _plan._nextRenderedListener %~ (*> hdl)
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'

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
    liftIO $ putStrLn "LOUISDEBUG: execRegisterDOMListener"
    obj' <- deRefWeakObj obj
    let mdlRef = modelRef obj'
        mdlVar = modelVar obj'
    -- Add the handler to the state
    UnliftIO u <- lift askUnliftIO
    -- generate a unique id
    k <- execMkReactId n
    liftIO $ do
        scn <- takeMVar mdlVar
        -- since k is unique, it'll always be a new map item
        -- update the ioref with the new handler
        listenerRef <- newIORef mempty
        cb <- mkEventCallback listenerRef
        addEventHandler goStrict (u . executor . goLazy) listenerRef
        -- prepare the updated state,
        let scn' = scn & _plan._domlListeners.at k .~ (Just (cb, listenerRef))
                & _plan._finalCleanup %~ (*> removeDomListener j n cb)
        -- Update the subject
        atomicWriteIORef mdlRef scn'
        putMVar mdlVar scn'
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
