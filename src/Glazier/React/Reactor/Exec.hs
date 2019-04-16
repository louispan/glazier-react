{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor.Exec
    ( ReactorEnv(..)
    , mkReactorEnvIO
    , startApp
    , execReactorCmd
    , execMkReactId
    , execSetRender
    , execMkObj
    , execGetElementalRef
    , execRerender
    , execRerenderNow
    , execMutate
    , execNotifyMutated
    , execResetMutation
    , execRegisterDOMListener
    , execRegisterReactListener
    , execRegisterMountedListener
    , execRegisterRenderedListener
    , execRegisterRenderedOnceListener
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
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.JSString as J
import qualified Data.JSString.Text as J
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple
import Data.Typeable
import GHC.Stack
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Foreign.Export as J
import qualified GHCJS.Types as J
import Glazier.Benign
import Glazier.Command
import Glazier.Logger
import Glazier.Logger.Exec
import Glazier.React.Entity
import Glazier.React.EventTarget
import Glazier.React.Gadget
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Obj
import Glazier.React.Obj.Internal
import Glazier.React.ReactDOM
import Glazier.React.ReactId.Internal
import Glazier.React.Reactor
import Glazier.React.Shim
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE
import System.IO
import System.Mem.Weak

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

data ReactorEnv = ReactorEnv
    { reactIdVar :: MVar Int
    , defaultLogLevel :: IORef (Maybe LogLevel)  -- Nothing means turn off logging
    -- overrides defaultLogLevel.
    -- This map will be populated with entries are Obj with different lognames are created.
    -- Nothing means no override.
    -- Just Nothing means turn off logging.
    -- Do not delete from this map to remove log overrides
    -- instead, set them to nothing.
    , logLevelOverrides :: IORef (HM.HashMap T.Text (IORef (Maybe (Maybe LogLevel))))
    }

makeLenses_ ''ReactorEnv

mkReactorEnvIO :: Maybe LogLevel -> HM.HashMap T.Text (IORef (Maybe (Maybe LogLevel)))
    -> IO (ReactorEnv)
mkReactorEnvIO logLvl overrides = ReactorEnv <$> (newMVar (0 :: Int))
    <*> (newIORef logLvl)
    <*> (newIORef overrides)

namedLogLevel :: IORef (Maybe LogLevel) -> IORef (HM.HashMap T.Text (IORef (Maybe (Maybe LogLevel))))
    -> T.Text -> IO (Benign IO (Maybe LogLevel))
namedLogLevel defLogLvlRef logLvlOverridesRef logname = do
    -- find or insert entry into overrides
    defOverride <- newIORef Nothing
    entryRef <- atomicModifyIORef' logLvlOverridesRef (swap . findOrInsert logname defOverride)
    -- return action to read from overrides
    let go = do
            override <- benignReadIORef entryRef
            case override of
                -- override is Nothing, ready from defLogLvl IORef
                Nothing -> benignReadIORef defLogLvlRef
                Just x -> pure x
    pure go

-- findOrInsert :: (Eq k, At (map k)) => k -> v -> map k v -> (v, map k v)
findOrInsert k v = at k . non v <%~ id

-- | An example of starting an app using the glazier-react framework
-- This requies a (IO c) executor to create an MVar to store the app object.
startApp ::
    ( MonadIO m
    , Typeable s -- for J.export
    , AsReactor c
    , Logger r c m
    , AsFacet (IO c) c
    )
    => (c -> m ()) -> Widget c s s () -> J.JSString -> s -> JE.JSRep -> m ()
startApp executor wid logname s root = do
    logLvl <- view _logLevel
    -- create a mvar to store the app object
    objVar <- liftIO $ newEmptyMVar
    let setup = do
            obj <- mkObj' wid logname s
            -- Create an 'IO c' to store the created object into an mvar
            exec' (command_ <$> (putMVar objVar obj))
        cs = (`execState` mempty) $ runProgramT $ evalContT $ (`runReaderT` logLvl) setup

    -- run the initial commands, this will store the app obj into objVar
    traverse_ executor cs

    -- Start the App render
    liftIO $ do
        obj <- takeMVar objVar
        markup <- getBenign $ (`execStateT` mempty) $ displayObj obj
        e <- toElement markup
        renderDOM e root

        -- Export obj to prevent it from being garbage collected
        -- objVar is no longer needed, let it fall out of scope
        void $ J.export obj


-- | Returns commands that need to be processed last
execReactorCmd ::
    ( MonadUnliftIO m
    , MonadReader r m
    , AsReactor c
    , Has ReactorEnv r
    )
    => (c -> m ()) -> ReactorCmd c -> m [c]
execReactorCmd executor c = case c of
    MkReactId n k -> done $ execMkReactId n >>= (executor . k)
    SetRender obj w -> done $ execSetRender obj w
    MkObj wid logname s k -> done $ execMkObj executor wid logname s >>= (executor . k)
    GetReactRef obj k f -> done $ execGetReactRef executor obj k f
    ScheduleRerender obj -> execScheduleRerender obj
    RerenderNow obj -> done $ execRerenderNow obj
    Mutate obj k tick -> (`evalMaybeT` []) $ do
        (lastCmds, nextCmd) <- execMutate obj k tick
        lift $ executor nextCmd
        pure lastCmds
    NotifyMutated obj k -> execNotifyMutated obj k
    ResetMutation obj k -> execResetMutation obj k
    RegisterDOMListener obj j n goStrict goLazy -> done $ execRegisterDOMListener executor obj j n goStrict goLazy
    RegisterReactListener obj k n goStrict goLazy -> done $ execRegisterReactListener executor obj k n goStrict goLazy
    RegisterMountedListener obj k -> done $ execRegisterMountedListener executor obj k
    RegisterRenderedListener obj k -> done $ execRegisterRenderedListener executor obj k
    RegisterRenderedOnceListener obj k -> done $ execRegisterRenderedOnceListener executor obj k
    RegisterMutatedListener obj k -> done $ execRegisterMutatedListener executor obj k
  where
    done f = (\() -> []) <$> f


-- -----------------------------------------------------------------
-- execLogLn ::
--     ( MonadIO m
--     , Has ReactorEnv r
--     , MonadReader r m
--     )
--     => CallStack -> LogLevel -> Benign IO J.JSString -> m ()
-- execLogLn stk lvl m = do
--     defLvlVar <- view ((hasLens @ReactorEnv)._defaultLogLevel)
--     mdlLvlsVar <- view ((hasLens @ReactorEnv)._moduleLogLevel)
--     liftIO $ do
--         mLvl <- runMaybeT $ do
--             mdl <- MaybeT $ pure mFstModule
--             mdlLvls <- lift $ readIORef mdlLvlsVar
--             MaybeT $ pure $ HM.lookup mdl mdlLvls
--         allowedLvl <- maybe (readIORef defLvlVar) pure mLvl
--         when (lvl >= allowedLvl) $ case lvl of
--             LogTrace -> liftIO $ getBenign m >>= (go js_logInfo  "TRACE " $ prettyStk fstStk)
--             LogDebug -> liftIO $ getBenign m >>= (go js_logInfo  "DEBUG " $ prettyStk fstStk)
--             LogInfo -> liftIO $ getBenign m >>=  (go js_logInfo  "INFO  " $ prettyStk fstStk)
--             LogWarn -> liftIO $ getBenign m >>=  (go js_logWarn  "WARN  " $ prettyStk fstStk)
--             LogError -> liftIO $ getBenign m >>= (go js_logError "ERROR " $ prettyStk stks)
--   where
--     go f hdr ftr a = f $ hdr <> a <> ftr
--     stks = getCallStack stk
--     mFstStk = listToMaybe stks
--     mFstSrcLoc = snd <$> mFstStk
--     mFstModule = srcLocModule <$> mFstSrcLoc
--     fstStk = maybeToList $ mFstStk
--     prettyStk s = J.pack $ " [" <> prettyCallStack' s <> "]"

--     -- Modified from GHC.Stack to be shorter
--     prettyCallStack' :: [(String, SrcLoc)] -> String
--     prettyCallStack' = intercalate "\n " . prettyCallStackLines'

--     prettyCallStackLines' :: [(String, SrcLoc)] -> [String]
--     prettyCallStackLines' = fmap prettyCallSite'

--     prettyCallSite' :: (String, SrcLoc) -> String
--     prettyCallSite' (f, loc) = f ++ "@" ++ prettySrcLoc' loc

--     prettySrcLoc' :: SrcLoc -> String
--     prettySrcLoc' SrcLoc {..}
--         = foldr (++) ""
--             [ srcLocModule, ":"
--             , show srcLocStartLine, ":"
--             , show srcLocStartCol
--             ]

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
        pure $ ReactId (n, i')

execSetRender :: MonadIO m => WeakObj s -> Window s () -> m ()
execSetRender obj win = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execSetRender"
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    -- create the callbacks
    liftIO $ do
        renderCb <- J.syncCallback' (renderCb obj win)
        -- Replace the existing ShimCallbacks
        scn <- takeMVar scnVar
        -- replace the rendering function
        let origRenderCb = scn ^. _plan._shimCallbacks._shimOnRender
            scn' = scn & _plan._shimCallbacks._shimOnRender .~ renderCb
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        J.releaseCallback origRenderCb
  where
    renderCb :: WeakObj s -> Window s () -> IO J.JSVal
    renderCb obj win = (`evalMaybeT` J.nullRef) $ do
        obj' <- deRefWeakObj obj
        let scnRef = sceneRef obj'
        lift $ do
            -- render using from scnRef (doesn't block)
            scn <- readIORef scnRef
            (mrkup, _) <- getBenign (RWS.execRWST win scn mempty) -- ignore unit writer output
            a <- JE.toJS <$> toElement mrkup
            pure a

-- | Make an initialized 'Obj' for a given model using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
-- 'displayObj' should be used to render the subject.
execMkObj ::
    ( MonadIO m
    , Has ReactorEnv r
    , MonadReader r m
    , AsReactor c
    )
    => (c -> m ())
    -> Widget c s s ()
    -> J.JSString
    -> s
    -> m (Obj s)
execMkObj executor wid logname s = do
    liftIO $ putStrLn "LOUISDEBUG: execMkObj"
    defLogLvlRef <- view ((hasLens @ReactorEnv)._defaultLogLevel)
    logLvlOverridesRef <- view ((hasLens @ReactorEnv)._logLevelOverrides)
    k <- execMkReactId logname
    (obj, cs) <- liftIO $ do
        namedLogLvl <- namedLogLevel defLogLvlRef logLvlOverridesRef (J.textFromJSString logname)
        -- create shim with fake callbacks for now
        let newPlan = Plan
                k
                namedLogLvl
                Nothing
                (ShimCallbacks (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef) (J.Callback J.nullRef))
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                S.empty
                RerenderNotRequired
            scn = Model newPlan s

        scnRef <- newIORef scn
        scnVar <- newEmptyMVar

        mdlWkRef <- mkWeakIORef scnRef $
            putStrLn "LOUISDEBUG: release scnRef"

        -- Create automatic garbage collection of the callbacks
        -- that will run when the Obj is garbage collected.
        mdlWkVar <- mkWeakMVar scnVar $ do
            putStrLn "LOUISDEBUG: release scnVar"
            scn' <- readIORef scnRef
            scn' ^. _plan._finalCleanup
            releasePlanCallbacks (scn' ^. _plan)

        let obj = WeakObj mdlWkRef mdlWkVar
        -- Create callbacks for now
        renderCb <- J.syncCallback' (pure J.nullRef) -- dummy render for now
        refCb <- J.syncCallback1 J.ContinueAsync (onRefCb obj)
        mountedCb <- J.syncCallback J.ContinueAsync (onMountedCb obj)
        renderedCb <- J.syncCallback J.ContinueAsync (onRenderedCb obj)

        -- Now we have enough to make a subject
        let gad = runExceptT wid
            gad' = gad >>= either (fmap Left . setRndr) (pure . Right)
            gad'' = (either id id) <$> gad'
            tick = runProgramT $ runGadget gad'' (Entity id obj) pure
            cs = execState tick mempty
            -- update the model to include the real shimcallbacks
            scn' = scn & _plan._shimCallbacks .~ ShimCallbacks renderCb mountedCb renderedCb refCb
        -- update the mutable variables with the initialzed model
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'
        pure (Obj obj scnRef scnVar, cs)
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

    onRefCb :: WeakObj s -> J.JSVal -> IO ()
    onRefCb obj j = (`evalMaybeT` ()) $ do
        obj' <- deRefWeakObj obj
        let scnRef = sceneRef obj'
            scnVar = sceneVar obj'
        lift $ do
            -- update componentRef held in the Plan
            scn <- takeMVar scnVar
            let scn' = scn & _plan._componentRef .~ (JE.fromJS j)
            atomicWriteIORef scnRef scn'
            putMVar scnVar scn'

    onRenderedCb :: WeakObj s -> IO ()
    onRenderedCb obj = (`evalMaybeT` ()) $ do
        obj' <- deRefWeakObj obj
        let scnRef = sceneRef obj'
            scnVar = sceneVar obj'
        lift $ do
            -- update renderedOnceListener held in the Plan
            scn <- takeMVar scnVar
            let scn' = scn & _plan._renderedOnceListener .~ mempty
                nxt = scn ^. _plan._renderedOnceListener
                cb = scn ^. _plan._renderedListener

            atomicWriteIORef scnRef scn'
            putMVar scnVar scn'
            nxt
            cb

    onMountedCb :: WeakObj s -> IO ()
    onMountedCb obj = (`evalMaybeT` ()) $ do
        scnRef <- MaybeT . deRefWeak $ sceneWeakRef obj
        lift $ do
            scn <- readIORef scnRef
            scn ^. _plan._mountedListener

-- Schedule a rerender, only if not lalready scheduled or
-- not in middle of mutation
execScheduleRerender ::
    ( MonadIO m
    , AsReactor c
    )
    => WeakObj s -> m [c]
execScheduleRerender obj = (`evalMaybeT` []) $ do
    liftIO $ putStrLn "LOUISDEBUG: execScheduleRerender"
    obj' <- deRefWeakObj obj
    let scnVar = sceneVar obj'
    liftIO $ do
        scn <- takeMVar scnVar
        liftIO $ putStrLn "LOUISDEBUG: execScheduleRerender2"
        let (cs, scn') = (`runState` scn) $ scheduleRerender obj
        putMVar scnVar scn'
        pure cs

scheduleRerender ::
    ( AsReactor c
    )
    => WeakObj s -> State (Model s) [c]
scheduleRerender alsoRerenderSuppressed obj = do
    scn <- get
    -- don't rerender straight away, but schedule a rerender
    -- so multiple rerender requests will be naturally throttled
    if scn ^. _plan._rerendering == RerenderNotRequired
        && scn ^. _plan._mutations.to S.null
        then do
            put $ scn & _plan._rerendering .~ RerenderRequired
            -- Schedule a rerender
            pure [command' $ RerenderNow obj]
        -- rerender has already been scheduled
        else pure []

execRerenderNow :: MonadIO m => WeakObj s -> m ()
execRerenderNow obj = (`evalMaybeT` ()) $ do
    liftIO $ putStrLn "LOUISDEBUG: execRerenderNow"
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    liftIO $ do
        scn <- takeMVar scnVar
        liftIO $ putStrLn "LOUISDEBUG: execRerenderNow2"
        -- don't render if in the middle of a mutation
        -- (the ResetMutation will render)
        -- Or if rerenderScheduled is false - it means rerender has been called
        if scn ^. _plan._rerendering == RerenderRequired
            && scn ^. _plan._mutations.to S.null
            then do
                let scn' = scn & _plan._rerendering .~ RerenderNotRequired
                -- Update the back buffer
                atomicWriteIORef scnRef scn'
                putMVar scnVar scn'
                putStrLn $ "LOUISDEBUG: execRerenderNow3a "
                case scn ^. _plan._componentRef of
                    Nothing -> pure ()
                    Just j -> rerenderShim j
            -- rerender not required (eg. already processed, or in the middle of mutation)
            else do
                putMVar scnVar scn
                putStrLn $ "LOUISDEBUG: execRerenderNow3b "

-- | No need to run in a separate thread because it should never block for a significant amount of time.
-- Update the model 'MVar' with the given action. Also triggers a rerender.
-- Returns (cmds to process last, cmds to process first)
execMutate ::
    ( MonadIO m
    , AsReactor c
    )
    => WeakObj s
    -> ReactId
    -> SceneState s c
    -> MaybeT m ([c], c)
execMutate obj k tick = do
    liftIO $ putStrLn $ "LOUISDEBUG: execMutate " <> J.unpack (reactIdKey k)
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    -- q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
    liftIO $ do
        scn <- takeMVar scnVar
        putStrLn $ "LOUISDEBUG: execMutate2 " <> J.unpack (reactIdKey k)
        let s = scn ^. _model
        (c, s') <- getBenign $ runStateT tick s
        let scn' = scn & _model .~ s'
        -- don't notify ticked listener straight away, but schedule it like rerender.
        -- This is so that if there are multiple mutates, it will try to only fire
        -- the notified callback as late as possible.
        -- 'execResetMutation' is now responsible for triggering a rerender,
        -- so suppress rerender until after 'execResetMutation' is called.
        let scn'' = scn' & _plan._rerendering .~ RerenderSuppressed
            notifyCmd = [command' $ NotifyMutated obj k]
            -- Only fire NotifyMutated once for the same ReactId in processing cycle.
            (c', scn''') = case S.member k (scn' ^. _plan._mutations) of
                False -> (notifyCmd, scn'' & _plan._mutations %~ (S.insert k))
                True -> ([], scn'')
        -- LOUISFIXME: remove debug
        case S.member k (scn' ^. _plan._mutations) of
            False -> putStrLn $ "new mutation"
            True -> putStrLn $ "old mutation"
        -- Update the back buffer
        atomicWriteIORef scnRef scn'''
        putMVar scnVar scn'''
        putStrLn $ "LOUISDEBUG: execMutate3 " <> J.unpack (reactIdKey k)
        pure (c', c)

-- LOUISFIXME: Document tickNotified/renderRequired lifecycle
-- ensuring that the mutatedListener callback is called.
-- at most once per reactorBackgroundBatch
-- We want to cater for the case if two obj ticked and listen to each other
-- then we won't get infinite work
-- to also schedule a rerender
-- Returns commands to process last
-- There should only be one execNotifyMutated per ReactId from multiple Mutate with the same ReactId
execNotifyMutated ::
    ( MonadIO m
    , AsReactor c
    )
    => WeakObj s -> ReactId -> m [c]
execNotifyMutated obj k = (`evalMaybeT` []) $ do
    liftIO $ putStrLn $ "LOUISDEBUG: execNotifyMutated " <> J.unpack (reactIdKey k)
    obj' <- deRefWeakObj obj
    let scnVar = sceneVar obj'
    liftIO $ do
        scn <- readMVar scnVar
        putStrLn $ "LOUISDEBUG: execNotifyMutated2 " <> J.unpack (reactIdKey k)
        if S.member k (scn ^. _plan._mutations)
            then do
                -- Don't reset back to NotMutated to avoid
                -- infinite loops if the mutatedListener mutates this same object
                let cb = scn ^. _plan._mutatedListener
                -- run mutatedListener
                cb k
                -- schedule reset mutation and rerendering
                pure [command' $ ResetMutation obj k]
            -- notify not required (eg. already processed)
            else pure []

-- LOUISFIXME: Document tickNotified/renderRequired lifecycle
-- There should only be one execNotifyMutated per ReactId from multiple Mutate with the same ReactId
execResetMutation ::
    ( MonadIO m
    , AsReactor c
    )
    => WeakObj s -> ReactId -> m [c]
execResetMutation obj k = (`evalMaybeT` []) $ do
    liftIO $ putStrLn $ "LOUISDEBUG: execResetMutation " <> J.unpack (reactIdKey k)
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    liftIO $ do
        scn <- takeMVar scnVar
        putStrLn $ "LOUISDEBUG: execResetMutation2 " <> J.unpack (reactIdKey k)
        let scn' = scn & _plan._mutations %~ (S.delete k)
            -- If this was the last ResetMutation, then rerender
            (cs, scn'') = if scn' ^. _plan._mutations.to S.null
                then (`runState` scn') $ scheduleRerender obj
                else ([], scn')
        -- Update the back buffer
        atomicWriteIORef scnRef scn''
        putMVar scnVar scn''
        putStrLn $ "LOUISDEBUG: execResetMutation3 " <> J.unpack (reactIdKey k)
        pure cs

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

execGetReactRef ::
    ( MonadUnliftIO m
    )
    => (c -> m ())
    -> WeakObj s
    -> ReactId
    -> (EventTarget -> c)
    -> m ()
execGetReactRef executor obj k f = (`evalMaybeT` ()) $ do
    liftIO $ putStrLn "LOUISDEBUG: execGetReactRef"
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    scn <- liftIO $ takeMVar scnVar
    (refFreshness, pln) <- lift $ getOrRegisterRefCoreListener obj k (plan scn)
    let tryAgain = u $ execGetReactRef executor obj k f
        scn' = scn & _plan .~ pln -- to save the core listener
                & _plan._renderedOnceListener %~ (*> tryAgain)
        ret = pln ^? (_elementals.ix k._elementalRef._Just)
        triggerTryAgain = liftIO $ do
            liftIO $ putStrLn "LOUISDEBUG: triggerTryAgain"
            -- save the tryAgain handler when nextRendered
            atomicWriteIORef scnRef scn'
            putMVar scnVar scn'
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
                liftIO $ putMVar scnVar scn
                lift . executor $ f ret'

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
                    liftIO $ putStrLn $ "LOUISDEBUG: RefCore Nothing " <> J.unpack (reactIdKey k)
                    listenerRef <- newIORef mempty
                    cb <- mkEventCallback listenerRef
                    -- update listenerRef with new event listener
                    -- only do this once (when for first ref listener)
                    addEventHandler (pure . JE.fromJSR) hdlRef listenerRef
                    pure (Fresh, (cb, listenerRef))
                Just eventHdl -> do
                    liftIO $ putStrLn $ "LOUISDEBUG: RefCore Just " <> J.unpack (reactIdKey k)
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
        let scnRef = sceneRef obj'
            scnVar = sceneVar obj'
        lift $ do
            scn <- takeMVar scnVar
            let scn' = scn & _plan._elementals.ix k._elementalRef .~ x
            -- Update the back buffer
            atomicWriteIORef scnRef scn'
            putMVar scnVar scn'

execRegisterReactListener :: (NFData a, MonadUnliftIO m)
    => (c -> m ())
    -> WeakObj s
    -> ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> c)
    -> m ()
execRegisterReactListener executor obj k n goStrict goLazy = void . runMaybeT $ do
    liftIO $ putStrLn $ "LOUISDEBUG: execRegisterReactListener " <> J.unpack (reactIdKey k)
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    scn_ <- liftIO $ takeMVar scnVar
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
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

mappendListener :: (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ())
mappendListener (f1, g1) (f2, g2) = (\x -> f1 x *> f2 x, g1 *> g2)

execRegisterMutatedListener :: (MonadUnliftIO m)
    => (c -> m ())
    -> WeakObj s
    -> (ReactId -> c)
    -> m ()
execRegisterMutatedListener executor obj f = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterMutatedListener"
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u . executor . f
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._mutatedListener %~ (\g -> \k -> g k *> hdl k)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

execRegisterMountedListener :: (MonadUnliftIO m)
    => (c -> m ())
    -> WeakObj s
    -> c
    -> m ()
execRegisterMountedListener executor obj c = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterMountedListener"
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._mountedListener %~ (*> hdl)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

execRegisterRenderedListener :: (MonadUnliftIO m)
    => (c -> m ())
    -> WeakObj s
    -> c
    -> m ()
execRegisterRenderedListener executor obj c = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterRenderedListener"
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._renderedListener %~ (*> hdl)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

execRegisterRenderedOnceListener :: (MonadUnliftIO m)
    => (c -> m ())
    -> WeakObj s
    -> c
    -> m ()
execRegisterRenderedOnceListener executor obj c = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterRenderedOnceListener"
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        scn <- takeMVar scnVar
        let scn' = scn & _plan._renderedOnceListener %~ (*> hdl)
        atomicWriteIORef scnRef scn'
        putMVar scnVar scn'

execRegisterDOMListener ::
    ( NFData a
    , MonadUnliftIO m
    , Has ReactorEnv r
    , MonadReader r m
    )
    => (c -> m ())
    -> WeakObj s
    -> JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> c)
    -> m ()
execRegisterDOMListener executor obj j n goStrict goLazy = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterDOMListener"
    obj' <- deRefWeakObj obj
    let scnRef = sceneRef obj'
        scnVar = sceneVar obj'
    -- Add the handler to the state
    UnliftIO u <- lift askUnliftIO
    -- generate a unique id
    k <- execMkReactId n
    liftIO $ do
        scn <- takeMVar scnVar
        -- since k is unique, it'll always be a new map item
        -- update the ioref with the new handler
        listenerRef <- newIORef mempty
        cb <- mkEventCallback listenerRef
        addEventHandler goStrict (u . executor . goLazy) listenerRef
        -- prepare the updated state,
        let scn' = scn & _plan._domlListeners.at k .~ (Just (cb, listenerRef))
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

foreign import javascript unsafe
    "console.info($1);"
    js_logInfo :: J.JSString -> IO ()

foreign import javascript unsafe
    "console.warn($1);"
    js_logWarn :: J.JSString -> IO ()

foreign import javascript unsafe
    "console.error($1);"
    js_logError :: J.JSString -> IO ()

#else

js_addDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()
js_addDomListener _ _ _ = pure mempty

js_removeDomListener :: J.JSVal -> J.JSString -> J.JSVal -> IO ()
js_removeDomListener _ _ _ = pure mempty

js_logInfo :: J.JSString -> IO ()
js_logInfo = putStrLn . J.unpack

js_logWarn :: J.JSString -> IO ()
js_logWarn = hPutStrLn stderr . J.unpack

js_logError :: J.JSString -> IO ()
js_logError = hPutStrLn stderr . J.unpack

#endif
