{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor.Exec where

import Control.Also
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Benign
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.JSString as J
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple
import Data.Typeable
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Foreign.Export as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Command.Exec
import Glazier.Logger
import Glazier.React.EventTarget
import Glazier.React.Markup
import Glazier.React.ReactDOM
import Glazier.React.ReactId
import Glazier.React.Reactor
import Glazier.React.Model
import Glazier.React.Shim
import Glazier.React.Widget
-- import Glazier.React.Widget.Internal
import qualified JavaScript.Extras as JE
import System.IO
import System.Mem.Weak

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

data ReactorEnv = ReactorEnv
    { uniqueId :: IORef Int
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

reactIdLogName :: ReactId -> T.Text
reactIdLogName (ReactId (n NE.:| ns, _)) = T.intercalate "." . reverse $ (T.pack . J.unpack) <$> (n : ns)

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
  where
    findOrInsert :: (At t, Eq (IxValue t)) => Index t -> IxValue t -> t -> (IxValue t, t)
    findOrInsert k v = at k . non v <%~ id

-- | renders the given obj onto the given javascript dom
-- and exports the obj to prevent it from being garbage collected
-- which means the "main" haskell thread can exit.
startModelRef :: (MonadIO m, Typeable s) => JE.JSRep -> ModelRef s -> m (J.Export (ModelRef s))
startModelRef root ref = liftIO $ do
    mdl <- readIORef ref
    markup <- (`execStateT` mempty) $ displayModel ref
    e <- toElement markup
    renderDOM e root

    -- Export obj to prevent it from being garbage collected
    J.export ref

-- | An example of starting an app using the glazier-react framework
-- WARN: A different @Obj o@ will be create everytime this function is used,
-- however, each time running this may execute arbitrary commands in the given
-- widget in order to initialize the widget object.
-- This function has a redundant constraint of @CmdTypes c c ~ CmdTypes (NoIOCmd c) c@
-- which ensures that the command @c@ doesn't contain arbitrary @IO c@ effects.
startWidget ::
    ( MonadIO m
    , Has ReactorEnv r
    , MonadReader r m
    , AsReactor c
    , Typeable s
    -- redundant contraint, but ensures no @IO c@ commands can be executed
    , CmdTypes c c ~ CmdTypes (NoIOCmd c) c
    )
    => (c -> m ()) -> Widget (Gizmo c s) () -> NE.NonEmpty J.JSString -> s -> JE.JSRep -> m (J.Export (Obj s))
startWidget executor wid logname s root = execMkObj executor wid logname s >>= startObj root

-- | Returns commands that need to be processed last
execReactorCmd ::
    ( MonadUnliftIO m
    , MonadBenignIO m
    , MonadReader r m
    , AsReactor c
    , Has ReactorEnv r
    )
    => (c -> m ()) -> ReactorCmd c -> m [c]
execReactorCmd executor c = case c of
    MkReactId n k -> done $ execMkReactId n >>= (executor . k)
    MkEventHandler goStrict goLazy k -> done $ execMkEventHandler executor goStrict goLazy >>= (executor . k)
    -- SetRender obj w -> done $ execSetRender obj w
    SetPrerendered obj w -> done $ execSetPrerendered obj w
    MkModelRef wid logname s k -> done $ execMkModelRef executor wid logname s >>= (executor . k)
    GetReactRef obj k f -> done $ execGetReactRef executor obj k f
    ScheduleRerender obj -> execScheduleRerender obj
    RerenderNow obj -> done $ execRerenderNow obj
    Mutate obj k tick -> (`evalMaybeT` []) $ do
        (lastCmds, nextCmd) <- execMutate obj k tick
        lift $ executor nextCmd
        pure lastCmds
    NotifyMutated obj k -> execNotifyMutated obj k
    ResetMutation obj k -> execResetMutation obj k
    -- RegisterDOMListener obj j n goStrict goLazy -> done $ execRegisterDOMListener executor obj j n goStrict goLazy

    -- callback on each html node level
    RegisterReactListener obj k n goStrict goLazy -> done $ execRegisterReactListener executor obj k n goStrict goLazy

    -- callback on the whole widget level
    RegisterMountedListener obj k -> done $ execRegisterMountedListener executor obj k
    RegisterRenderedListener obj k -> done $ execRegisterRenderedListener executor obj k
    -- RegisterRenderedOnceListener obj k -> done $ execRegisterRenderedOnceListener executor obj k
    RegisterMutatedListener obj k -> done $ execRegisterMutatedListener executor obj k
  where
    done f = (\() -> []) <$> f

-- -- -----------------------------------------------------------------
-- -- execLogLn ::
-- --     ( MonadIO m
-- --     , Has ReactorEnv r
-- --     , MonadReader r m
-- --     )
-- --     => CallStack -> LogLevel -> Benign IO J.JSString -> m ()
-- -- execLogLn stk lvl m = do
-- --     defLvlVar <- view ((hasLens @ReactorEnv)._defaultLogLevel)
-- --     mdlLvlsVar <- view ((hasLens @ReactorEnv)._moduleLogLevel)
-- --     liftIO $ do
-- --         mLvl <- runMaybeT $ do
-- --             mdl <- MaybeT $ pure mFstModule
-- --             mdlLvls <- lift $ readIORef mdlLvlsVar
-- --             MaybeT $ pure $ HM.lookup mdl mdlLvls
-- --         allowedLvl <- maybe (readIORef defLvlVar) pure mLvl
-- --         when (lvl >= allowedLvl) $ case lvl of
-- --             LogTrace -> liftIO $ getBenign m >>= (go js_logInfo  "TRACE " $ prettyStk fstStk)
-- --             LogDebug -> liftIO $ getBenign m >>= (go js_logInfo  "DEBUG " $ prettyStk fstStk)
-- --             LogInfo -> liftIO $ getBenign m >>=  (go js_logInfo  "INFO  " $ prettyStk fstStk)
-- --             LogWarn -> liftIO $ getBenign m >>=  (go js_logWarn  "WARN  " $ prettyStk fstStk)
-- --             LogError -> liftIO $ getBenign m >>= (go js_logError "ERROR " $ prettyStk stks)
-- --   where
-- --     go f hdr ftr a = f $ hdr <> a <> ftr
-- --     stks = getCallStack stk
-- --     mFstStk = listToMaybe stks
-- --     mFstSrcLoc = snd <$> mFstStk
-- --     mFstModule = srcLocModule <$> mFstSrcLoc
-- --     fstStk = maybeToList $ mFstStk
-- --     prettyStk s = J.pack $ " [" <> prettyCallStack' s <> "]"

-- --     -- Modified from GHC.Stack to be shorter
-- --     prettyCallStack' :: [(String, SrcLoc)] -> String
-- --     prettyCallStack' = intercalate "\n " . prettyCallStackLines'

-- --     prettyCallStackLines' :: [(String, SrcLoc)] -> [String]
-- --     prettyCallStackLines' = fmap prettyCallSite'

-- --     prettyCallSite' :: (String, SrcLoc) -> String
-- --     prettyCallSite' (f, loc) = f ++ "@" ++ prettySrcLoc' loc

-- --     prettySrcLoc' :: SrcLoc -> String
-- --     prettySrcLoc' SrcLoc {..}
-- --         = foldr (++) ""
-- --             [ srcLocModule, ":"
-- --             , show srcLocStartLine, ":"
-- --             , show srcLocStartCol
-- --             ]

execMkReactId ::
    ( MonadIO m
    , Has ReactorEnv r
    , MonadReader r m
    )
    => NE.NonEmpty J.JSString
    -> m ReactId
execMkReactId n = do
    v <- view ((hasLens @ReactorEnv)._uniqueId)
    j <- liftIO $ atomicModifyIORef' v go
    pure $ ReactId (n, j)
  where
    go i = let j = JE.safeIncrement i in (j, j)

-- execSetRender :: (MonadIO m, MonadBenignIO m) => WeakObj s -> Window s () -> m ()
-- execSetRender obj win = void . runMaybeT $ do
--     liftIO $ putStrLn "LOUISDEBUG: execSetRender"
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     -- create the callbacks
--     liftIO $ do
--         cb <- J.syncCallback' (renderCb obj win)
--         -- Replace the existing ShimCallbacks
--         mdl <- takeMVar mdlVar
--         -- replace the rendering function
--         let origRenderCb = mdl ^. _plan._shimCallbacks._shimOnRender
--             mdl' = mdl & _plan._shimCallbacks._shimOnRender .~ cb
--         atomicWriteIORef mdlRef mdl'
--         putMVar mdlVar mdl'
--         J.releaseCallback origRenderCb
--   where
--     renderCb :: WeakObj s -> Window s () -> IO J.JSVal
--     renderCb o wn = (`evalMaybeT` J.nullRef) $ do
--         obj' <- benignDeRefWeakObj o
--         let mdlRef = sceneRef obj'
--         lift $ do
--             -- render using from mdlRef (doesn't block)
--             mdl <- readIORef mdlRef
--             (mrkup, _) <- getBenign (RWS.execRWST wn mdl mempty) -- ignore unit writer output
--             a <- JE.toJS <$> toElement mrkup
--             pure a


execSetPrerendered :: (MonadIO m) => WeakObj s -> DL.DList ReactMarkup -> m ()
execSetPrerendered obj mrkup = void . runMaybeT $ do
    frame <- liftIO $ JE.toJS <$> toElement mrkup
    liftIO $ putStrLn "LOUISDEBUG: execSetPrerendered"
    obj' <- MaybeT $ liftIO $ deRefWeak obj
    -- replace the prerendered frame
    let go mdl = (mdl & _plan._prerendered .~ frame, ())
    atomicModifyIORef' obj' go

execMkModelRef ::
    ( MonadIO m
    , MonadUnliftIO m,
    , MonadBenignIO m
    , Has ReactorEnv r
    , MonadReader r m
    , AsReactor c
    )
    => (c -> m ())
    -> Widget (Gizmo c s) ()
    -> NE.NonEmpty J.JSString
    -> s
    -> m (ModelRef s)
execMkModelRef executor (Widget wid) n s = do
    UnliftIO u <- askUnliftIO
    liftIO $ putStrLn "LOUISDEBUG: execMkObj"
    defLogLvlRef <- view ((hasLens @ReactorEnv)._defaultLogLevel)
    logLvlOverridesRef <- view ((hasLens @ReactorEnv)._logLevelOverrides)
    i <- execMkReactId n
    let logname = reactIdLogName i
    (obj, rerndr) <- liftIO $ do
        namedLogLvl <- namedLogLevel defLogLvlRef logLvlOverridesRef logname
        -- create shim with fake callbacks for now
        let newPlan = Plan
                i
                namedLogLvl
                Nothing
                (ShimCallbacks
                    (J.Callback J.nullRef)
                    (J.Callback J.nullRef)
                    (J.Callback J.nullRef)
                    (J.Callback J.nullRef)
                    (J.Callback J.nullRef))
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                mempty
                S.empty
                RerenderNotRequired
                J.nullRef
            mdl = Model newPlan s

        mdlRef <- newIORef mdl

        -- Create automatic garbage collection of the callbacks
        -- that will run when the Obj is garbage collected.
        wkScnRef <- mkWeakIORef mdlRef $
            putStrLn "LOUISDEBUG: release mdlRef"
            mdl' <- readIORef mdlRef
            mdl' ^. _plan._finalCleanup
            releasePlanCallbacks (mdl' ^. _plan)
            --FIXME createdCallbacks

        -- let obj = WeakObj mdlWkRef mdlWkVar
        -- Create callbacks for now
        renderCb <- J.syncCallback' (onRenderCb wkScnRef)
        refCb <- J.syncCallback1 J.ContinueAsync (onRefCb mdlwkScnRefRef)
        mountedCb <- J.syncCallback J.ContinueAsync (onMountedCb wkScnRef)
        renderedCb <- J.syncCallback J.ContinueAsync (onRenderedCb wkScnRef)
        -- update the plan to include the real shimcallbacks
        let mdl' = mdl & _plan._shimCallbacks .~ ShimCallbacks renderCb mountedCb renderedCb refCb
                & _plan._rerender = rerndr

        atomicWriteIORef obj mdl'

        -- Now we have enough to run the widget
        let wid' = do
                -- get the window out of wid and set the rendering function
                -- the window cannot be obtained from execStateT because it
                -- will return in the partial result due to StateT instance of 'codify'
                -- So use 'SetPrerendered' to store the final window.
                wid `also` (pure ())
                win <- askWindow
                wk <- askWeakModelRef
                exec' $ SetPrerendered wk win

            -- mkRerenderCs :: Benign IO (DL.DList c)
            mkRerenderCs = (`evalMaybeT` mempty) $ do
                -- get the latest state from the ioref
                ref <- MaybeT $ liftBenignIO $ Benign $ deRefWeak wk
                atomicModifyIORef' ref clearCbs
                s <- MaybeT $ liftBenignIO $ Benign $ readIORef ref
                -- then get the latest markup using the state
                execProgramT'
                    $ (`evalStateT` mempty)
                    $ (`evalStateT` i)
                    $ evalContT
                    $ (`evalMaybeT` ())
                    $ (`runReaderT` wkObj) -- FIXME: TODO: try not using weak here again
                    $ (`runReaderT` s) $ wid'
            rerndr = u $ executor (command' $ (commands . DL.toList) <$> mkRerenderCs) -- :: IO ()
        pure (obj, rerndr)

    -- init and update the widget window
    -- one of these commands will be 'SetPrerendered' which will
    -- update the prerendered markup given the latest state in @Obj s@.
    rerndr

    -- return the scene ref
    pure obj
  where
    clearCb :: Model s -> (Model s, ())
    clearCb mdl = mdl
        & _plan.mountedListeners .~ mempty
        & _plan.unmountedListeners .~ mempty
        & _plan.renderedListener .~ mempty
        & _plan.mutatedListener .~ mempty
    onRenderCb :: WeakModelRef s -> IO J.JSval
    onRenderCb wk = (`evalMaybeT` J.nullRef) $ do
        mdlRef <- MaybeT $ deRefWeak wk
        lift $ do
            mdl <- readIORef mdlRef
            pure $ mdl ^. _plan.prerendered

    onRefCb :: WeakObj s -> J.JSVal -> IO ()
    onRefCb wk j = (`evalMaybeT` ()) $ do
        mdlRef <- MaybeT $ deRefWeak wk
        -- update shimRef held in the Plan
        lift $ atomicModifyIORef' mdlRef (\mdl -> (mdl & _plan._shimRef .~ (JE.fromJS j), ()))

    -- FIXME: 
    onRenderedCb :: WeakObj s -> IO ()
    onRenderedCb obj = (`evalMaybeT` ()) $ do
        obj' <- benignDeRefWeakObj obj
        let mdlRef = sceneRef obj'
            mdlVar = sceneVar obj'
        lift $ do
            -- update renderedOnceListener held in the Plan
            mdl <- takeMVar mdlVar
            let mdl' = mdl & _plan._renderedOnceListener .~ mempty
                nxt = mdl ^. _plan._renderedOnceListener
                cb = mdl ^. _plan._renderedListener

            atomicWriteIORef mdlRef mdl'
            putMVar mdlVar mdl'
            nxt
            cb

    onMountedCb :: WeakObj s -> IO ()
    onMountedCb obj = (`evalMaybeT` ()) $ do
        mdlRef <- MaybeT . deRefWeak $ sceneWeakRef obj
        lift $ do
            mdl <- readIORef mdlRef
            mdl ^. _plan._mountedListener

-- Schedule a rerender, only if not lalready scheduled or
-- not in middle of mutation
execScheduleRerender ::
    ( MonadIO m
    , MonadBenignIO m
    , AsReactor c
    )
    => WeakObj s -> m [c]
execScheduleRerender obj = (`evalMaybeT` []) $ do
    liftIO $ putStrLn "LOUISDEBUG: execScheduleRerender"
    obj' <- benignDeRefWeakObj obj
    let mdlVar = sceneVar obj'
    liftIO $ do
        mdl <- takeMVar mdlVar
        liftIO $ putStrLn "LOUISDEBUG: execScheduleRerender2"
        let (cs, mdl') = (`runState` mdl) $ scheduleRerender obj
        putMVar mdlVar mdl'
        pure cs

scheduleRerender ::
    ( AsReactor c
    )
    => WeakObj s -> State (Model s) [c]
scheduleRerender obj = do
    mdl <- get
    -- don't rerender straight away, but schedule a rerender
    -- so multiple rerender requests will be naturally throttled
    if mdl ^. _plan._rerendering == RerenderNotRequired
        && mdl ^. _plan._mutations.to S.null
        then do
            put $ mdl & _plan._rerendering .~ RerenderScheduled
            -- Schedule a rerender
            pure [command' $ RerenderNow obj]
        -- rerender has already been scheduled
        else pure []

execRerenderNow :: (MonadIO m, MonadBenignIO m) => WeakObj s -> m ()
execRerenderNow obj = (`evalMaybeT` ()) $ do
    liftIO $ putStrLn "LOUISDEBUG: execRerenderNow"
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    liftIO $ do
        mdl <- takeMVar mdlVar
        liftIO $ putStrLn "LOUISDEBUG: execRerenderNow2"
        -- don't render if in the middle of a mutation
        -- (the ResetMutation will render)
        -- Or if RerenderScheduled is false - it means rerender has been already called
        if mdl ^. _plan._rerendering == RerenderScheduled
            && mdl ^. _plan._mutations.to S.null
            then do
                let mdl' = mdl & _plan._rerendering .~ RerenderNotRequired
                -- Update the back buffer
                atomicWriteIORef mdlRef mdl'
                putMVar mdlVar mdl'
                -- first update the prerendered frame
                putStrLn $ "LOUISDEBUG: execRerenderNow3a "
                mdl' ^. _plan._rerender
                --  notify javascript react that we are ready to rerender
                putStrLn $ "LOUISDEBUG: execRerenderNow3b "
                case mdl' ^. _plan._shimRef of
                    Nothing -> pure ()
                    Just j -> rerenderShim j
            -- rerender not required (eg. already processed, or in the middle of mutation)
            else do
                putMVar mdlVar mdl
                putStrLn $ "LOUISDEBUG: execRerenderNow3b "

-- | No need to run in a separate thread because it should never block for a significant amount of time.
-- Update the meta 'MVar' with the given action. Also triggers a rerender.
-- Returns (cmds to process last, cmds to process first)
execMutate ::
    ( MonadIO m
    , MonadBenignIO m
    , AsReactor c
    )
    => WeakObj s
    -> ReactId
    -> ModelState s c
    -> MaybeT m ([c], c)
execMutate obj k tick = do
    liftIO $ putStrLn $ "LOUISDEBUG: execMutate " <> show k
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    -- q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
    liftIO $ do
        mdl <- takeMVar mdlVar
        putStrLn $ "LOUISDEBUG: execMutate2 " <> show k
        let s = mdl ^. _meta
        (c, s') <- getBenign $ runStateT tick s
        let mdl' = mdl & _meta .~ s'
        -- don't notify ticked listener straight away, but schedule it like rerender.
        -- This is so that if there are multiple mutates, it will try to only fire
        -- the notified callback as late as possible.
        -- 'execResetMutation' is now responsible for triggering a rerender,
        -- so suppress rerender until after 'execResetMutation' is called.
        let mdl'' = mdl' & _plan._rerendering .~ RerenderScheduled
            notifyCmd = [command' $ NotifyMutated obj k]
            -- Only fire NotifyMutated once for the same ReactId in processing cycle.
            (c', mdl''') = case S.member k (mdl' ^. _plan._mutations) of
                False -> (notifyCmd, mdl'' & _plan._mutations %~ (S.insert k))
                True -> ([], mdl'')
        -- LOUISFIXME: remove debug
        case S.member k (mdl' ^. _plan._mutations) of
            False -> putStrLn $ "new mutation"
            True -> putStrLn $ "old mutation"
        -- Update the back buffer
        atomicWriteIORef mdlRef mdl'''
        putMVar mdlVar mdl'''
        putStrLn $ "LOUISDEBUG: execMutate3 " <> show k
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
    , MonadBenignIO m
    , AsReactor c
    )
    => WeakObj s -> ReactId -> m [c]
execNotifyMutated obj k = (`evalMaybeT` []) $ do
    liftIO $ putStrLn $ "LOUISDEBUG: execNotifyMutated " <> show k
    obj' <- benignDeRefWeakObj obj
    let mdlVar = sceneVar obj'
    liftIO $ do
        mdl <- readMVar mdlVar
        putStrLn $ "LOUISDEBUG: execNotifyMutated2 " <> show k
        if S.member k (mdl ^. _plan._mutations)
            then do
                -- Don't reset back to NotMutated to avoid
                -- infinite loops if the mutatedListener mutates this same object
                let cb = mdl ^. _plan._mutatedListener
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
    , MonadBenignIO m
    , AsReactor c
    )
    => WeakObj s -> ReactId -> m [c]
execResetMutation obj k = (`evalMaybeT` []) $ do
    liftIO $ putStrLn $ "LOUISDEBUG: execResetMutation " <> show k
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    liftIO $ do
        mdl <- takeMVar mdlVar
        putStrLn $ "LOUISDEBUG: execResetMutation2 " <> show k
        let mdl' = mdl & _plan._mutations %~ (S.delete k)
            -- If this was the last ResetMutation, then rerender
            (cs, mdl'') = if mdl' ^. _plan._mutations.to S.null
                then (`runState` mdl') $ scheduleRerender obj
                else ([], mdl')
        -- Update the back buffer
        atomicWriteIORef mdlRef mdl''
        putMVar mdlVar mdl''
        putStrLn $ "LOUISDEBUG: execResetMutation3 " <> show k
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
mkEventProcessor :: (NFData a) => (evt -> MaybeT IO a) -> IO (evt -> IO (), MaybeT IO a)
mkEventProcessor goStrict = do
    liftIO $ putStrLn "LOUISDEBUG: mkEventProcessor"
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
    (preprocessor, postprocessor) <- mkEventProcessor (goStrict . JE.toJSRep)
    let postprocessor' = (`evalMaybeT` ()) (postprocessor >>= (lift . goLazy))
    atomicModifyIORef' listenerRef $ \hdl -> (hdl `mappendListener` (preprocessor, postprocessor'), ())
  where
    mappendListener :: (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ())
    mappendListener (f1, g1) (f2, g2) = (\x -> f1 x *> f2 x, g1 *> g2)

execMkEventHandler :: (NFData a, MonadUnliftIO m, MonadBenignIO m)
    => (c -> m ())
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> c)
    -> m (J.JSVal -> IO (), IO ())
execMkEventHandler executor goStrict goLazy = do
    (preprocessor, postprocessor) <- liftIO $ mkEventProcessor (goStrict . JE.toJSRep)
    -- Add the handler to the state
    UnliftIO u <- askUnliftIO
    let postprocessor' = (u . executor . goLazy) <$> postprocessor
        postprocessor'' = join $ (`evalMaybeT` pure ()) postprocessor'
    pure (preprocessor, postprocessor'')

-- | Create ref handler to assign 'elementalRef'
-- FIXME" automatically add "ref" handler if any react listeners are created
execGetReactRef ::
    ( MonadUnliftIO m
    , MonadBenignIO m
    )
    => (c -> m ())
    -> WeakObj s
    -> ReactId
    -> (EventTarget -> c)
    -> m ()
execGetReactRef executor obj k f = (`evalMaybeT` ()) $ do
    liftIO $ putStrLn "LOUISDEBUG: execGetReactRef"
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO

    -- make sure the reactRef shim ref is registered, and get the possibly modified mdl
    (_, mdl) <- do
        mdl <- liftIO $ takeMVar mdlVar
        (`runStateT` mdl) $ getOrMkListenerRef obj k "ref"

     let ret = mdl ^? (_plan._reactants.ix k._reactRef._Just)
        tryAgain = u $ execGetReactRef executor obj k f
        tryAgainScn = mdl & _plan._renderedOnceListener %~ (*> tryAgain)

        -- This puts back the mvar
        putBackAndTryAgain = liftIO $ do
            liftIO $ putStrLn "LOUISDEBUG: triggerTryAgain"
            -- save the tryAgain handler when nextRendered
            atomicWriteIORef mdlRef tryAgainScn
            putMVar mdlVar tryAgainScn
            -- trigger a rerender immediately
            -- Note: componentRef should be set
            -- since it is part of the shimComponent wrapper
            -- which should have been initialized by now.
            -- In the very unlikely case it is not set
            -- (eg still initializing?) then there is nothing
            -- we can do for now.
            -- But as soon as the initialzation finishes, and renders,
            -- then the tryAgain callback will be called.
            case tryAgainScn ^. _plan._shimRef of
                Nothing -> pure ()
                Just j -> rerenderShim j
     case ret of
        Nothing -> do
            liftIO $ putStrLn "LOUISDEBUG: Existing Nothing"
            putBackAndTryAgain
        Just ret' ->do
            liftIO $ do
                atomicWriteIORef mdlRef mdl
                putMVar mdlVar mdl
            lift . executor $ f ret'

getOrMkListenerRef :: (MonadIO m)
    => WeakObj s
    -> ReactId
    -> J.JSString
    -> StateT (Model s) m (IORef (J.JSVal -> IO (), IO ()))
getOrMkListenerRef obj k n = do
    liftIO $ putStrLn $ "LOUISDEBUG: getOrRegisterListener " <> J.unpack n
    mdl <- get
    -- return the new eventHandler if it is new
    case mdl ^. _plan._reactants.at k.anon emptyRectant isEmptyReactant._reactListeners.at n of
        Nothing -> do
            eventHdl@(_, listenerRef) <- liftIO $ do
                listenerRef <- newIORef mempty
                cb <- mkEventCallback listenerRef
                pure (cb, listenerRef)

            -- Special case for "ref" listener.
            -- creates a "ref" listener to to set the react ref to 'reactRef'
            -- This is to support 'GetReactRef', in addition to explicit
            -- register callback to "ref" for user-defined purposes.
            -- The invariant is that if the "ref" listener is empty then
            -- GetReactRef was not called for the reactant.
            -- Else if the "ref" listener is non-empty then there must
            -- have been one registered to set the 'reactRef'.
            -- Then it is safe to add user defined "ref" callbacks.
            if (n == (J.pack "ref"))
                then liftIO $ addEventHandler (pure . JE.fromJSRep) hdlReactRef listenerRef
                else pure ()

            -- update the scene with the new handler
            put $ mdl & _plan._reactants.at k.anon emptyRectant isEmptyReactant._reactListeners.at n .~ Just eventHdl
            pure listenerRef
        Just (_, listenerRef) -> pure listenerRef
  where
    emptyRectant = Reactant Nothing mempty
    isEmptyReactant (Reactant r ls) = isNothing r && M.null ls
    hdlReactRef x = void . runMaybeT $ do
        obj' <- benignDeRefWeakObj obj
        let mdlRef = sceneRef obj'
            mdlVar = sceneVar obj'
        lift $ do
            mdl <- takeMVar mdlVar
            let mdl' = mdl & _plan._reactants.ix k._reactRef .~ x
            -- Update the back buffer
            atomicWriteIORef mdlRef mdl'
            putMVar mdlVar mdl'

execRegisterReactListener :: (NFData a, MonadUnliftIO m, MonadBenignIO m)
    => (c -> m ())
    -> WeakObj s
    -> ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> c)
    -> m ()
execRegisterReactListener executor obj k n goStrict goLazy = void . runMaybeT $ do
    liftIO $ putStrLn $ "LOUISDEBUG: execRegisterReactListener " <> show k
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    mdl <- liftIO $ takeMVar mdlVar
    (listenerRef, mdl') <- (`runStateT` mdl) $ getOrMkListenerRef obj k n
    liftIO $ do
        -- update listenerRef with new event listener
        addEventHandler goStrict (u . executor . goLazy) listenerRef
        -- Update the subject
        atomicWriteIORef mdlRef mdl'
        putMVar mdlVar mdl'

execRegisterMutatedListener :: (MonadUnliftIO m, MonadBenignIO m)
    => (c -> m ())
    -> WeakObj s
    -> (ReactId -> c)
    -> m ()
execRegisterMutatedListener executor obj f = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterMutatedListener"
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u . executor . f
    liftIO $ do
        mdl <- takeMVar mdlVar
        let mdl' = mdl & _plan._mutatedListener %~ (\g -> \k -> g k *> hdl k)
        atomicWriteIORef mdlRef mdl'
        putMVar mdlVar mdl'

execRegisterMountedListener :: (MonadUnliftIO m, MonadBenignIO m)
    => (c -> m ())
    -> WeakObj s
    -> c
    -> m ()
execRegisterMountedListener executor obj c = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterMountedListener"
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        mdl <- takeMVar mdlVar
        let mdl' = mdl & _plan._mountedListener %~ (*> hdl)
        atomicWriteIORef mdlRef mdl'
        putMVar mdlVar mdl'

execRegisterRenderedListener :: (MonadUnliftIO m, MonadBenignIO m)
    => (c -> m ())
    -> WeakObj s
    -> c
    -> m ()
execRegisterRenderedListener executor obj c = void . runMaybeT $ do
    liftIO $ putStrLn "LOUISDEBUG: execRegisterRenderedListener"
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    UnliftIO u <- lift askUnliftIO
    let hdl = u $ executor c
    liftIO $ do
        mdl <- takeMVar mdlVar
        let mdl' = mdl & _plan._renderedListener %~ (*> hdl)
        atomicWriteIORef mdlRef mdl'
        putMVar mdlVar mdl'

-- execRegisterRenderedOnceListener :: (MonadUnliftIO m, MonadBenignIO m)
--     => (c -> m ())
--     -> WeakObj s
--     -> c
--     -> m ()
-- execRegisterRenderedOnceListener executor obj c = void . runMaybeT $ do
--     liftIO $ putStrLn "LOUISDEBUG: execRegisterRenderedOnceListener"
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     UnliftIO u <- lift askUnliftIO
--     let hdl = u $ executor c
--     liftIO $ do
--         mdl <- takeMVar mdlVar
--         let mdl' = mdl & _plan._renderedOnceListener %~ (*> hdl)
--         atomicWriteIORef mdlRef mdl'
--         putMVar mdlVar mdl'

execRegisterDOMListener ::
    ( NFData a
    , MonadUnliftIO m
    , MonadBenignIO m
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
    obj' <- benignDeRefWeakObj obj
    let mdlRef = sceneRef obj'
        mdlVar = sceneVar obj'
    -- Add the handler to the state
    UnliftIO u <- lift askUnliftIO
    -- generate a unique id
    k <- execMkReactId (n NE.:| [])
    liftIO $ do
        mdl <- takeMVar mdlVar
        -- since k is unique, it'll always be a new map item
        -- update the ioref with the new handler
        listenerRef <- newIORef mempty
        cb <- mkEventCallback listenerRef
        addEventHandler goStrict (u . executor . goLazy) listenerRef
        -- prepare the updated state,
        let mdl' = mdl & _plan._domlListeners.at k .~ (Just (cb, listenerRef))
                & _plan._finalCleanup %~ (*> removeDomListener j n cb)
        -- Update the subject
        atomicWriteIORef mdlRef mdl'
        putMVar mdlVar mdl'
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
