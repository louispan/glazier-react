{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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

import Control.Concurrent.MVar
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Context
import Control.Monad.IO.Unlift
import Control.Monad.Observer
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import qualified Data.DList as DL
import Data.Function.Extras
import qualified Data.HashMap.Strict as HM
import Data.IORef.Extras
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.String
import Data.Tagged.Extras
import Data.Tuple
import GHC.Stack
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Markup
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.ReactBatch
import Glazier.React.ReactId.Internal
import Glazier.React.Reactor
import Glazier.React.ReactPath
import Glazier.React.Shim
import Glazier.React.Widget
import qualified JavaScript.Extras as JE
import System.IO
import System.Mem.Weak

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- data ReactorEnv = ReactorEnv
-- nextReactId :: ReactId
--     ,

data LogConfig = LogConfig
    { defaultLogLevel :: Maybe LogLevel  -- Nothing means turn off logging
    -- Nothing means don't change defaults from loglevel
    -- Just Nothing means full stack
    , defaultLogCallStackDepth :: Maybe (Maybe LogCallStackDepth)
    -- overrides defaultLogLevel.
    -- This map will be populated with override for different lognames
    -- Do not delete from this map to remove log overrides
    -- instead, set them to nothing.
    , logOverrides :: HM.HashMap LogName (IORef
        -- Nothing means no override.
        -- Just Nothing means turn off logging.
        ( Maybe (Maybe LogLevel)
        -- Nothing means no override.
        -- Just Nothing means full stack
        , Maybe (Maybe LogCallStackDepth)))
    }

makeLenses_ ''LogConfig

type AskLogConfigRef = MonadAsk (IORef LogConfig)
askLogConfigRef :: AskLogConfigRef m => m (IORef LogConfig)
askLogConfigRef = askContext

-- | returns io actions that will always have latest log state for the logname
getLogConfig :: (MonadIO m, AskLogConfigRef m)=> LogName -> m (IO (Maybe LogLevel), IO (Maybe (Maybe LogCallStackDepth)))
getLogConfig logname = do
    envRef <- askLogConfigRef
    -- find or insert entry into overrides
    newOverride <- liftIO $ newIORef (Nothing, Nothing)
    entryRef <- liftIO $ atomicModifyIORef' envRef $ \env ->
        let (b, overrides) = findOrInsert logname newOverride (logOverrides env)
        in (env {logOverrides = overrides}, b)
    -- return action to read from overrides
    let getLogLevel = do
            (override, _) <- readIORef entryRef
            case override of
                -- override is Nothing, read default from envRef
                Nothing -> defaultLogLevel <$> readIORef envRef
                Just x -> pure x
        getLogDepth = do
            (_, override) <- readIORef entryRef
            case override of
                -- override is Nothing, read default from envRef
                Nothing -> defaultLogCallStackDepth <$> readIORef envRef
                Just x -> pure $ Just x
    pure (getLogLevel, getLogDepth)

-----------------------------------------------

type AskNextReactIdRef = MonadAsk (IORef (Tagged "NextReactId" ReactId))
askNextReactIdRef :: AskNextReactIdRef m => m (IORef (Tagged "NextReactId" ReactId))
askNextReactIdRef = askContext

mkReactId :: (MonadIO m, AskNextReactIdRef m) => m ReactId
mkReactId = do
    ref <- askNextReactIdRef
    liftIO $ atomicModifyIORef' ref $ \n ->
        let ReactId i = untag' @"NextReactId" n
        in ( Tagged @"NextReactId" . ReactId $ i + 1, ReactId i)

-----------------------------------------------

type AskDirtyPlan = MonadAsk (Tagged "DirtyPlan" (IORef (M.Map ReactId (Weak (IORef Plan)))))
askDirtyPlan :: AskDirtyPlan m => m (Tagged "DirtyPlan" (IORef (M.Map ReactId (Weak (IORef Plan)))))
askDirtyPlan = askContext

type AskReactBatch = MonadAsk ReactBatch
askReactBatch :: AskReactBatch m => m ReactBatch
askReactBatch = askContext

rerenderDirtyPlans :: (AskReactBatch m, AskDirtyPlan m, MonadIO m) => m ()
rerenderDirtyPlans = do
    ref <- untag' @"DirtyPlan" <$> askDirtyPlan
    ds <- liftIO $ atomicModifyIORef' ref $ \ds -> (mempty, ds)
    liftIO $ foldMap prerndr ds >>= liftIO -- ^ possibly async GHCJS
    -- by this time, there is a possibilty that shms were removed,
    -- only only batch shms that are still valid
    btch <- askReactBatch
    liftIO $ foldMap (batchShim btch) ds >>= liftIO -- ^ possibly async GHCJS
    -- now run the batch tell react to use the prerendered frames
    liftIO $ runReactBatch btch
  where
    prerndr plnWkRef = (`evalMaybeT` (pure ())) $ do
        plnRef <- MaybeT $ liftIO $ deRefWeak plnWkRef
        liftIO $ prerender <$> readIORef plnRef
    batchShim btch plnWkRef = (`evalMaybeT` (pure ())) $ do
        plnRef <- MaybeT $ liftIO $ deRefWeak plnWkRef
        shm <- maybeM $ liftIO $ shimRef <$> readIORef plnRef
        pure $ batchShimRerender shm btch




    -- liftIO $ batchShimRerender btch shm
    -- on the first time this is called, shimRef will be Nothing
    -- because the Shim is not mounted yet
    -- but the pre-rendered frame will be ready when it is mounted


-- execRerender :: (AskReactBatch m, MonadIO m) => Weak (IORef Plan) -> m ()
-- execRerender wk = (`evalMaybeT` ()) $ do
--     fixme $ liftIO $ putStrLn "LOUISDEBUG: execRerender"
--     plnRef <- MaybeT $ liftIO $ deRefWeak wk
--     pln <- liftIO $ readIORef plnRef
--     fixme $ liftIO $ putStrLn "LOUISDEBUG: execRerender2"
--     liftIO $ case rerenderRequired pln of
--         (Nothing, _) -> pure ()
--         (_, RerenderNotRequired) -> pure ()
--         (Just shm, RerenderRequired) -> rerenderShim shm

        -- replace the prerendered frame
-- -- | renders the given obj onto the given javascript dom
-- -- and exports the obj to prevent it from being garbage collected
-- -- which means the "main" haskell thread can exit.
-- startModelRef :: (MonadIO m, Typeable s) => JE.JSRep -> ModelRef s -> m (J.Export (ModelRef s))
-- startModelRef root ref = liftIO $ do
--     mdl <- readIORef ref
--     markup <- (`execStateT` mempty) $ displayModel ref
--     e <- toElement markup
--     renderDOM e root

--     -- Export obj to prevent it from being garbage collected
--     J.export ref

-- -- | An example of starting an app using the glazier-react framework
-- -- WARN: A different @Obj o@ will be create everytime this function is used,
-- -- however, each time running this may execute arbitrary commands in the given
-- -- widget in order to initialize the widget object.
-- startWidget ::
--     ( MonadIO m
--     , Has ReactorEnv r
--     , MonadReader r m
--     , AsReactor c
--     , Typeable s
--     )
--     => (c -> m ()) -> Widget c s () -> NE.NonEmpty J.JSString -> s -> JE.JSRep -> m (J.Export (Obj s))
-- startWidget executor wid logname s root = execMkObj executor wid logname s >>= startObj root

-- -- | Returns commands that need to be processed last
-- execReactorCmd ::
--     ( MonadUnliftIO m
--     , MonadBenignIO m
--     , MonadReader r m
--     , AsReactor c
--     , Has ReactorEnv r
--     )
--     => (c -> m ()) -> ReactorCmd c -> m [c]
-- execReactorCmd executor c = case c of
--     MkReactId n k -> done $ execMkReactId n >>= (executor . k)
--     MkEventHandler goStrict goLazy k -> done $ execMkEventHandler executor goStrict goLazy >>= (executor . k)
--     -- SetRender obj w -> done $ execSetRender obj w
--     SetPrerendered obj w -> done $ execSetPrerendered obj w
--     MkModelRef wid logname s k -> done $ execMkModelRef executor wid logname s >>= (executor . k)
--     GetReactRef obj k f -> done $ execGetReactRef executor obj k f
--     ScheduleRerender obj -> execScheduleRerender obj
--     RerenderNow obj -> done $ execRerenderNow obj
--     Mutate obj k tick -> (`evalMaybeT` []) $ do
--         (lastCmds, nextCmd) <- execMutate obj k tick
--         lift $ executor nextCmd
--         pure lastCmds
--     NotifyMutated obj k -> execNotifyMutated obj k
--     ResetMutation obj k -> execResetMutation obj k
--     -- RegisterDOMListener obj j n goStrict goLazy -> done $ execRegisterDOMListener executor obj j n goStrict goLazy

--     -- callback on each html node level
--     RegisterReactListener obj k n goStrict goLazy -> done $ execRegisterReactListener executor obj k n goStrict goLazy

--     -- callback on the whole widget level
--     RegisterMountedListener obj k -> done $ execRegisterMountedListener executor obj k
--     RegisterRenderedListener obj k -> done $ execRegisterRenderedListener executor obj k
--     -- RegisterRenderedOnceListener obj k -> done $ execRegisterRenderedOnceListener executor obj k
--     RegisterMutatedListener obj k -> done $ execRegisterMutatedListener executor obj k
--   where
--     done f = (\() -> []) <$> f

-----------------------------------------------------------------
execLogLineJS ::
    MonadIO m
    => LogLevel -> Tagged "LogName" J.JSString -> IO J.JSString -> [(String, SrcLoc)] -> m ()
execLogLineJS lvl n msg cs = do
    let f = case lvl of
            TRACE -> js_logInfo
            DEBUG -> js_logInfo
            INFO_ -> js_logInfo
            WARN_ -> js_logInfo
            ERROR -> js_logError
    liftIO $ msg >>= go f
  where
    go g a = g $ (fromString $ show lvl) <> " " <> (untag' @"LogName" n) <> " " <> a <> prettyCs
    prettyCs = case prettyCallStack' "; " cs of
        Nothing -> ""
        Just cs' -> " [" <> cs' <> "]"

execMkObj ::
    ( MonadIO m
    , MonadUnliftIO m
    , AskLogConfigRef m
    , AskNextReactIdRef m
    , AskReactBatch m
    , CmdReactor c
    )
    => (c -> m ())
    -> Widget c s ()
    -> LogName
    -> s
    -> m (Obj s)
execMkObj executor wid logName' s = do
    UnliftIO u <- askUnliftIO
    fixme $ liftIO $ putStrLn "LOUISDEBUG: execMkObj"
    (logLevel', logDepth') <- getLogConfig logName'
    i <- mkReactId
    btch <- askReactBatch
    (obj, prerndr) <- liftIO $ do
        -- create shim with fake callbacks for now
        let newPlan = Plan
                i
                logName'
                logLevel'
                logDepth'
                Nothing -- ^ himRef
                J.nullRef -- ^ prerendered, null for now
                mempty -- ^ prerender
                RerenderRequired -- ^ prerendered is null
                mempty -- ^ rendered
                mempty -- ^ final cleanup
                (ShimCallbacks
                    (J.Callback J.nullRef)
                    (J.Callback J.nullRef)
                    (J.Callback J.nullRef))

        -- scoped block to return just obj to avoid accidently using the strong refs
        obj <- liftIO $ do
            plnRef <- newIORef newPlan
            mdlVar <- newMVar s
            -- Create automatic garbage collection of the callbacks
            -- that will run when the Obj is garbage collected.
            plnWkRef <- mkWeakIORef plnRef $ do
                fixme $ putStrLn "LOUISDEBUG: release plnRef"
                pln <- readIORef plnRef
                destructor pln
                releasePlanCallbacks pln
            mdlWkVar <- mkWeakMVar mdlVar (pure ())

            pure $ Obj plnRef mdlVar $ WeakObj plnWkRef mdlWkVar

        -- Now we have enough to run the widget
        let Obj _ _ (WeakObj plnWkRef mdlWkVar) = obj
            wid' = do
                -- only run if 'RerenderRequired'
                plnRef' <- maybeM $ liftIO $ deRefWeak plnWkRef
                req <- liftIO $ atomicModifyIORef' plnRef' $ \pln ->
                    swap (pln & _rerenderRequired <<.~ RerenderNotRequired)

                case req of
                    RerenderNotRequired -> pure ()
                    RerenderRequired -> do
                        wid
                        -- get the window out of wid and set the rendering function
                        -- the window cannot be obtained from execStateT because it
                        -- will return in the partial result due to StateT instance of 'codify'
                        -- So use 'SetPrerendered' to store the final window.
                        ml <- askMarkup
                        setPrerendered plnWkRef ml

            -- mkRerenderCmds :: (c -> m ()) -> IO (DL.DList c)
            mkRerenderCmds onRendrd onDestruct onConstruct = (`evalMaybeT` mempty) $ do
                -- get the latest state from the weak ref
                mdlVar' <- MaybeT $ liftIO $ deRefWeak mdlWkVar
                mdl <- liftIO $ readMVar mdlVar'
                -- then get the latest markup using the state
                liftIO $ execProgramT'
                    $ (`evalStateT` mempty) -- markup
                    $ evalContT
                    $ (`evalMaybeT` ())
                    $ (`runReaderT` Tagged @"Model" mdl)
                    $ (`runReaderT` Tagged @"Model" mdlWkVar)
                    $ (`runReaderT` plnWkRef)
                    $ (`runReaderT` (ReactPath (Nothing, [])))
                    $ (`runObserverT` onConstruct)
                    $ (`runObserverT` onDestruct)
                    $ (`runObserverT` onRendrd)
                    $ wid'
            -- prerndr :: (c -> m ()) -> IO ()
            prerndr onRendrd onDestruct onConstruct = do
                c <- (commands . DL.toList) <$> (mkRerenderCmds onRendrd onDestruct onConstruct)
                u $ executor c
        pure (obj, prerndr)

    -- Create callbacks
    let plnWkRef = planWeakRef $ weakObj obj
        -- onXXXX :: MonadIO m => c -> m ()
        onConstruct = instruct . untag' @"Constructor"
        onDestruct c = (`evalMaybeT` ()) $ do
            plnRef' <- MaybeT $ liftIO $ deRefWeak plnWkRef
            liftIO $ atomicModifyIORef_' plnRef'
                $ _destructor %~ (<> (u $ executor $ untag' @"Destructor" c))
        onRendrd c = (`evalMaybeT` ()) $ do
            plnRef' <- MaybeT $ liftIO $ deRefWeak plnWkRef
            liftIO $ atomicModifyIORef_' plnRef'
                $ _rendered %~ (<> (u $ executor $ untag' @"Rendered" c))

    renderCb <- liftIO . J.syncCallback' $ onRenderCb plnWkRef
    refCb <- liftIO . J.syncCallback1 J.ContinueAsync $ onRefCb plnWkRef
    renderedCb <- liftIO . J.syncCallback J.ContinueAsync $ onRenderedCb plnWkRef

    -- update the plan to include the real shimcallbacks and prerender functon
    liftIO $ atomicModifyIORef_' (planRef obj) $ \pln ->
        (pln
            { shimCallbacks = ShimCallbacks renderCb refCb renderedCb
            -- ^ when rerendering, don't do anything during onConstruction calls
            , prerender = prerndr (const $ pure ()) (const $ pure ()) (const $ pure ())
            })

    -- run the prerender function for the first time, allowing onConstruction calls.
    -- This will also initialize the widget, and set the rendered frame in the Plan
    liftIO $ prerndr onRendrd onDestruct onConstruct

    -- We do not want explicitly tell React to show the prendered frame right now.
    -- This is the responsiblity of the caller of MkObj, usually via mutate

    -- return the obj created
    pure obj

  where
    setPrerendered :: MonadIO m => Weak (IORef Plan) -> DL.DList ReactMarkup -> m ()
    setPrerendered plnWkRef mrkup = (`evalMaybeT` ()) $ do
        frame <- liftIO $ JE.toJS <$> toElement mrkup
        fixme $ liftIO $ putStrLn "LOUISDEBUG: execSetPrerendered"
        plnRef <- MaybeT $ liftIO $ deRefWeak plnWkRef
        -- replace the prerendered frame
        liftIO $ atomicModifyIORef_' plnRef $ \pln ->
            (pln & _prerendered .~ frame)

    onRenderCb :: Weak (IORef Plan) -> IO J.JSVal
    onRenderCb wk = (`evalMaybeT` J.nullRef) $ do
        plnRef <- MaybeT $ deRefWeak wk
        liftIO $ do
            pln <- readIORef plnRef
            -- prerendered is guaranteed to be ready because the widget
            -- should have been initialized before being mounted
            pure $ prerendered pln

    onRefCb :: Weak (IORef Plan) -> J.JSVal -> IO ()
    onRefCb wk j = (`evalMaybeT` ()) $ do
        plnRef <- MaybeT $ deRefWeak wk
        liftIO $ atomicModifyIORef_' plnRef (_shimRef .~ JE.fromJS j)

    onRenderedCb :: Weak (IORef Plan) -> IO ()
    onRenderedCb wk = (`evalMaybeT` ()) $ do
        plnRef <- MaybeT $ deRefWeak wk
        liftIO $ readIORef plnRef >>= rendered

-- execMutate ::
--     ( MonadIO m
--     , MonadBenignIO m
--     , AsReactor c
--     )
--     => Weak (IORef Plan)
--     -> WeakRef s
--     -> ReactPath
--     -> RerenderRequired
--     -> StateT s IO c
--     -> m ()
-- execMutate plnWk mdlWk pth req tick = do
--     liftIO $ putStrLn $ "LOUISDEBUG: execMutate " <> show k
--     mdlVar <- MaybeT $ deRefWeak mdlWk
--     s <- takeMVar mdlVar
--     (c, s') <- liftIO $ runStateT tick s
--     putMVar mdlVar s'

--     -- FIXME: Need to run extra c

--     -- FIXME: then if pln has rerender dirty, batch
--     -- FIXME: and also notify


--     plnRef <- MaybeT $ deRefWeak wk

--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     -- q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
--     liftIO $ do
--         mdl <- takeMVar mdlVar
--         putStrLn $ "LOUISDEBUG: execMutate2 " <> show k
--         let s = mdl ^. _meta
--         (c, s') <- getBenign $ runStateT tick s
--         let mdl' = mdl & _meta .~ s'
--         -- don't notify ticked listener straight away, but schedule it like rerender.
--         -- This is so that if there are multiple mutates, it will try to only fire
--         -- the notified callback as late as possible.
--         -- 'execResetMutation' is now responsible for triggering a rerender,
--         -- so suppress rerender until after 'execResetMutation' is called.
--         let mdl'' = mdl' & _plan._rerendering .~ RerenderScheduled
--             notifyCmd = [command' $ NotifyMutated obj k]
--             -- Only fire NotifyMutated once for the same ReactId in processing cycle.
--             (c', mdl''') = case S.member k (mdl' ^. _plan._mutations) of
--                 False -> (notifyCmd, mdl'' & _plan._mutations %~ (S.insert k))
--                 True -> ([], mdl'')
--         -- LOUISFIXME: remove debug
--         case S.member k (mdl' ^. _plan._mutations) of
--             False -> putStrLn $ "new mutation"
--             True -> putStrLn $ "old mutation"
--         -- Update the back buffer
--         atomicWriteIORef mdlRef mdl'''
--         putMVar mdlVar mdl'''
--         putStrLn $ "LOUISDEBUG: execMutate3 " <> show k
--         pure (c', c)

-- -- | No need to run in a separate thread because it should never block for a significant amount of time.
-- -- Update the meta 'MVar' with the given action. Also triggers a rerender.
-- -- Returns (cmds to process last, cmds to process first)
-- execMutate ::
--     ( MonadIO m
--     , MonadBenignIO m
--     , AsReactor c
--     )
--     => WeakObj s
--     -> ReactId
--     -> ModelState s c
--     -> MaybeT m ([c], c)
-- execMutate obj k tick = do
--     liftIO $ putStrLn $ "LOUISDEBUG: execMutate " <> show k
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     -- q <- view ((hasLens @ReactorEnv)._reactorBackgroundEnv)
--     liftIO $ do
--         mdl <- takeMVar mdlVar
--         putStrLn $ "LOUISDEBUG: execMutate2 " <> show k
--         let s = mdl ^. _meta
--         (c, s') <- getBenign $ runStateT tick s
--         let mdl' = mdl & _meta .~ s'
--         -- don't notify ticked listener straight away, but schedule it like rerender.
--         -- This is so that if there are multiple mutates, it will try to only fire
--         -- the notified callback as late as possible.
--         -- 'execResetMutation' is now responsible for triggering a rerender,
--         -- so suppress rerender until after 'execResetMutation' is called.
--         let mdl'' = mdl' & _plan._rerendering .~ RerenderScheduled
--             notifyCmd = [command' $ NotifyMutated obj k]
--             -- Only fire NotifyMutated once for the same ReactId in processing cycle.
--             (c', mdl''') = case S.member k (mdl' ^. _plan._mutations) of
--                 False -> (notifyCmd, mdl'' & _plan._mutations %~ (S.insert k))
--                 True -> ([], mdl'')
--         -- LOUISFIXME: remove debug
--         case S.member k (mdl' ^. _plan._mutations) of
--             False -> putStrLn $ "new mutation"
--             True -> putStrLn $ "old mutation"
--         -- Update the back buffer
--         atomicWriteIORef mdlRef mdl'''
--         putMVar mdlVar mdl'''
--         putStrLn $ "LOUISDEBUG: execMutate3 " <> show k
--         pure (c', c)

-- -- LOUISFIXME: Document tickNotified/renderRequired lifecycle
-- -- ensuring that the mutatedListener callback is called.
-- -- at most once per reactorBackgroundBatch
-- -- We want to cater for the case if two obj ticked and listen to each other
-- -- then we won't get infinite work
-- -- to also schedule a rerender
-- -- Returns commands to process last
-- -- There should only be one execNotifyMutated per ReactId from multiple Mutate with the same ReactId
-- execNotifyMutated ::
--     ( MonadIO m
--     , MonadBenignIO m
--     , AsReactor c
--     )
--     => WeakObj s -> ReactId -> m [c]
-- execNotifyMutated obj k = (`evalMaybeT` []) $ do
--     liftIO $ putStrLn $ "LOUISDEBUG: execNotifyMutated " <> show k
--     obj' <- benignDeRefWeakObj obj
--     let mdlVar = sceneVar obj'
--     liftIO $ do
--         mdl <- readMVar mdlVar
--         putStrLn $ "LOUISDEBUG: execNotifyMutated2 " <> show k
--         if S.member k (mdl ^. _plan._mutations)
--             then do
--                 -- Don't reset back to NotMutated to avoid
--                 -- infinite loops if the mutatedListener mutates this same object
--                 let cb = mdl ^. _plan._mutatedListener
--                 -- run mutatedListener
--                 cb k
--                 -- schedule reset mutation and rerendering
--                 pure [command' $ ResetMutation obj k]
--             -- notify not required (eg. already processed)
--             else pure []

-- -- LOUISFIXME: Document tickNotified/renderRequired lifecycle
-- -- There should only be one execNotifyMutated per ReactId from multiple Mutate with the same ReactId
-- execResetMutation ::
--     ( MonadIO m
--     , MonadBenignIO m
--     , AsReactor c
--     )
--     => WeakObj s -> ReactId -> m [c]
-- execResetMutation obj k = (`evalMaybeT` []) $ do
--     liftIO $ putStrLn $ "LOUISDEBUG: execResetMutation " <> show k
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     liftIO $ do
--         mdl <- takeMVar mdlVar
--         putStrLn $ "LOUISDEBUG: execResetMutation2 " <> show k
--         let mdl' = mdl & _plan._mutations %~ (S.delete k)
--             -- If this was the last ResetMutation, then rerender
--             (cs, mdl'') = if mdl' ^. _plan._mutations.to S.null
--                 then (`runState` mdl') $ scheduleRerender obj
--                 else ([], mdl')
--         -- Update the back buffer
--         atomicWriteIORef mdlRef mdl''
--         putMVar mdlVar mdl''
--         putStrLn $ "LOUISDEBUG: execResetMutation3 " <> show k
--         pure cs

-- mkEventCallback ::
--     (MonadIO m)
--     => IORef (J.JSVal -> IO (), IO ())
--     -> m (J.Callback (J.JSVal -> IO ()))
-- mkEventCallback hdlRef = do
--     liftIO $ J.syncCallback1 J.ContinueAsync $ \evt -> do
--         (preprocessor, postprocessor) <- readIORef hdlRef
--         -- first run all the non-blocking preprocessors
--         preprocessor evt
--         -- then run all the possibly blocking postprocessors
--         postprocessor

-- -- | Using the NFData idea from React/Flux/PropertiesAndEvents.hs
-- -- React re-uses Notice from a pool, which means it may no longer be valid if we lazily
-- -- parse it. However, we still want lazy parsing so we don't parse unnecessary fields.
-- -- Additionally, we don't want to block during the event handling.The reason this is a problem is
-- -- because Javascript is single threaded, but Haskell is lazy.
-- -- Therefore GHCJS threads are a strange mixture of synchronous and asynchronous threads,
-- -- where a synchronous thread might be converted to an asynchronous thread if a "black hole" is encountered.
-- -- See https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Concurrent.hs
-- -- This safe interface requires two input functions:
-- -- 1. a function to reduce Notice to a NFData. The handleEvent will ensure that the
-- -- NFData is forced which will ensure all the required fields from Synthetic event has been parsed.
-- -- This function must not block.
-- -- 2. a second function that uses the NFData. This function is allowed to block.
-- -- handleEvent results in a function that you can safely pass into 'GHC.Foreign.Callback.syncCallback1'
-- -- with 'GHCJS.Foreign.Callback.ContinueAsync'.
-- -- I have innovated further with the NFData idea to return two functions:
-- -- 1. (evt -> IO ()) function to preprocess the event, which is guaranteed to be non blocking.
-- -- 2. An IO () postprocessor function which may block.
-- -- This allows for multiple handlers for the same event to be processed safely,
-- -- by allowing a way for all the preprocessor handlers to run first before
-- -- running all of the postprocessor handlers.
-- mkEventProcessor :: (NFData a) => (evt -> MaybeT IO a) -> IO (evt -> IO (), MaybeT IO a)
-- mkEventProcessor goStrict = do
--     liftIO $ putStrLn "LOUISDEBUG: mkEventProcessor"
--     -- create a channel to write preprocessed data for the postprocessor
--     -- 'Chan' guarantees that the writer is never blocked by the reader.
--     -- There is only one reader/writer per channel.
--     c <- newTQueueIO
--     let preprocess evt = (`evalMaybeT` ()) $ do
--             r <- goStrict evt
--             -- This is guaranteed never to block
--             lift $ atomically $ writeTQueue c $!! r
--         -- there might not be a value in the chan
--         -- because the preprocessor might not have produced any values
--         postprocess = MaybeT $ atomically $ tryReadTQueue c
--     pure (preprocess, postprocess)

-- addEventHandler :: (NFData a)
--     => (JE.JSRep -> MaybeT IO a)
--     -> (a -> IO ())
--     -> IORef (J.JSVal -> IO (), IO ())
--     -> IO ()
-- addEventHandler goStrict goLazy listenerRef = do
--     -- update the ioref with the new handler
--     (preprocessor, postprocessor) <- mkEventProcessor (goStrict . JE.toJSRep)
--     let postprocessor' = (`evalMaybeT` ()) (postprocessor >>= (lift . goLazy))
--     atomicModifyIORef' listenerRef $ \hdl -> (hdl `mappendListener` (preprocessor, postprocessor'), ())
--   where
--     mappendListener :: (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ()) -> (J.JSVal -> IO (), IO ())
--     mappendListener (f1, g1) (f2, g2) = (\x -> f1 x *> f2 x, g1 *> g2)

-- execMkEventHandler :: (NFData a, MonadUnliftIO m, MonadBenignIO m)
--     => (c -> m ())
--     -> (JE.JSRep -> MaybeT IO a)
--     -> (a -> c)
--     -> m (J.JSVal -> IO (), IO ())
-- execMkEventHandler executor goStrict goLazy = do
--     (preprocessor, postprocessor) <- liftIO $ mkEventProcessor (goStrict . JE.toJSRep)
--     -- Add the handler to the state
--     UnliftIO u <- askUnliftIO
--     let postprocessor' = (u . executor . goLazy) <$> postprocessor
--         postprocessor'' = join $ (`evalMaybeT` pure ()) postprocessor'
--     pure (preprocessor, postprocessor'')

-- -- | Create ref handler to assign 'elementalRef'
-- -- FIXME" automatically add "ref" handler if any react listeners are created
-- execGetReactRef ::
--     ( MonadUnliftIO m
--     , MonadBenignIO m
--     )
--     => (c -> m ())
--     -> WeakObj s
--     -> ReactId
--     -> (EventTarget -> c)
--     -> m ()
-- execGetReactRef executor obj k f = (`evalMaybeT` ()) $ do
--     liftIO $ putStrLn "LOUISDEBUG: execGetReactRef"
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     UnliftIO u <- lift askUnliftIO

--     -- make sure the reactRef shim ref is registered, and get the possibly modified mdl
--     (_, mdl) <- do
--         mdl <- liftIO $ takeMVar mdlVar
--         (`runStateT` mdl) $ getOrMkListenerRef obj k "ref"

--      let ret = mdl ^? (_plan._reactants.ix k._reactRef._Just)
--         tryAgain = u $ execGetReactRef executor obj k f
--         tryAgainScn = mdl & _plan._renderedOnceListener %~ (*> tryAgain)

--         -- This puts back the mvar
--         putBackAndTryAgain = liftIO $ do
--             liftIO $ putStrLn "LOUISDEBUG: triggerTryAgain"
--             -- save the tryAgain handler when nextRendered
--             atomicWriteIORef mdlRef tryAgainScn
--             putMVar mdlVar tryAgainScn
--             -- trigger a rerender immediately
--             -- Note: componentRef should be set
--             -- since it is part of the shimComponent wrapper
--             -- which should have been initialized by now.
--             -- In the very unlikely case it is not set
--             -- (eg still initializing?) then there is nothing
--             -- we can do for now.
--             -- But as soon as the initialzation finishes, and renders,
--             -- then the tryAgain callback will be called.
--             case tryAgainScn ^. _plan._shimRef of
--                 Nothing -> pure ()
--                 Just j -> rerenderShim j
--      case ret of
--         Nothing -> do
--             liftIO $ putStrLn "LOUISDEBUG: Existing Nothing"
--             putBackAndTryAgain
--         Just ret' ->do
--             liftIO $ do
--                 atomicWriteIORef mdlRef mdl
--                 putMVar mdlVar mdl
--             lift . executor $ f ret'

-- getOrMkListenerRef :: (MonadIO m)
--     => WeakObj s
--     -> ReactId
--     -> J.JSString
--     -> StateT (Model s) m (IORef (J.JSVal -> IO (), IO ()))
-- getOrMkListenerRef obj k n = do
--     liftIO $ putStrLn $ "LOUISDEBUG: getOrRegisterListener " <> J.unpack n
--     mdl <- get
--     -- return the new eventHandler if it is new
--     case mdl ^. _plan._reactants.at k.anon emptyRectant isEmptyReactant._reactListeners.at n of
--         Nothing -> do
--             eventHdl@(_, listenerRef) <- liftIO $ do
--                 listenerRef <- newIORef mempty
--                 cb <- mkEventCallback listenerRef
--                 pure (cb, listenerRef)

--             -- Special case for "ref" listener.
--             -- creates a "ref" listener to to set the react ref to 'reactRef'
--             -- This is to support 'GetReactRef', in addition to explicit
--             -- register callback to "ref" for user-defined purposes.
--             -- The invariant is that if the "ref" listener is empty then
--             -- GetReactRef was not called for the reactant.
--             -- Else if the "ref" listener is non-empty then there must
--             -- have been one registered to set the 'reactRef'.
--             -- Then it is safe to add user defined "ref" callbacks.
--             if (n == (J.pack "ref"))
--                 then liftIO $ addEventHandler (pure . JE.fromJSRep) hdlReactRef listenerRef
--                 else pure ()

--             -- update the scene with the new handler
--             put $ mdl & _plan._reactants.at k.anon emptyRectant isEmptyReactant._reactListeners.at n .~ Just eventHdl
--             pure listenerRef
--         Just (_, listenerRef) -> pure listenerRef
--   where
--     emptyRectant = Reactant Nothing mempty
--     isEmptyReactant (Reactant r ls) = isNothing r && M.null ls
--     hdlReactRef x = void . runMaybeT $ do
--         obj' <- benignDeRefWeakObj obj
--         let mdlRef = sceneRef obj'
--             mdlVar = sceneVar obj'
--         lift $ do
--             mdl <- takeMVar mdlVar
--             let mdl' = mdl & _plan._reactants.ix k._reactRef .~ x
--             -- Update the back buffer
--             atomicWriteIORef mdlRef mdl'
--             putMVar mdlVar mdl'

-- execRegisterReactListener :: (NFData a, MonadUnliftIO m, MonadBenignIO m)
--     => (c -> m ())
--     -> WeakObj s
--     -> ReactId
--     -> J.JSString
--     -> (JE.JSRep -> MaybeT IO a)
--     -> (a -> c)
--     -> m ()
-- execRegisterReactListener executor obj k n goStrict goLazy = void . runMaybeT $ do
--     liftIO $ putStrLn $ "LOUISDEBUG: execRegisterReactListener " <> show k
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     UnliftIO u <- lift askUnliftIO
--     mdl <- liftIO $ takeMVar mdlVar
--     (listenerRef, mdl') <- (`runStateT` mdl) $ getOrMkListenerRef obj k n
--     liftIO $ do
--         -- update listenerRef with new event listener
--         addEventHandler goStrict (u . executor . goLazy) listenerRef
--         -- Update the subject
--         atomicWriteIORef mdlRef mdl'
--         putMVar mdlVar mdl'

-- execRegisterMutatedListener :: (MonadUnliftIO m, MonadBenignIO m)
--     => (c -> m ())
--     -> WeakObj s
--     -> (ReactId -> c)
--     -> m ()
-- execRegisterMutatedListener executor obj f = void . runMaybeT $ do
--     liftIO $ putStrLn "LOUISDEBUG: execRegisterMutatedListener"
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     UnliftIO u <- lift askUnliftIO
--     let hdl = u . executor . f
--     liftIO $ do
--         mdl <- takeMVar mdlVar
--         let mdl' = mdl & _plan._mutatedListener %~ (\g -> \k -> g k *> hdl k)
--         atomicWriteIORef mdlRef mdl'
--         putMVar mdlVar mdl'

-- execRegisterMountedListener :: (MonadUnliftIO m, MonadBenignIO m)
--     => (c -> m ())
--     -> WeakObj s
--     -> c
--     -> m ()
-- execRegisterMountedListener executor obj c = void . runMaybeT $ do
--     liftIO $ putStrLn "LOUISDEBUG: execRegisterMountedListener"
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     UnliftIO u <- lift askUnliftIO
--     let hdl = u $ executor c
--     liftIO $ do
--         mdl <- takeMVar mdlVar
--         let mdl' = mdl & _plan._mountedListener %~ (*> hdl)
--         atomicWriteIORef mdlRef mdl'
--         putMVar mdlVar mdl'

-- execRegisterRenderedListener :: (MonadUnliftIO m, MonadBenignIO m)
--     => (c -> m ())
--     -> WeakObj s
--     -> c
--     -> m ()
-- execRegisterRenderedListener executor obj c = void . runMaybeT $ do
--     liftIO $ putStrLn "LOUISDEBUG: execRegisterRenderedListener"
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     UnliftIO u <- lift askUnliftIO
--     let hdl = u $ executor c
--     liftIO $ do
--         mdl <- takeMVar mdlVar
--         let mdl' = mdl & _plan._renderedListener %~ (*> hdl)
--         atomicWriteIORef mdlRef mdl'
--         putMVar mdlVar mdl'

-- -- execRegisterRenderedOnceListener :: (MonadUnliftIO m, MonadBenignIO m)
-- --     => (c -> m ())
-- --     -> WeakObj s
-- --     -> c
-- --     -> m ()
-- -- execRegisterRenderedOnceListener executor obj c = void . runMaybeT $ do
-- --     liftIO $ putStrLn "LOUISDEBUG: execRegisterRenderedOnceListener"
-- --     obj' <- benignDeRefWeakObj obj
-- --     let mdlRef = sceneRef obj'
-- --         mdlVar = sceneVar obj'
-- --     UnliftIO u <- lift askUnliftIO
-- --     let hdl = u $ executor c
-- --     liftIO $ do
-- --         mdl <- takeMVar mdlVar
-- --         let mdl' = mdl & _plan._renderedOnceListener %~ (*> hdl)
-- --         atomicWriteIORef mdlRef mdl'
-- --         putMVar mdlVar mdl'

-- execRegisterDOMListener ::
--     ( NFData a
--     , MonadUnliftIO m
--     , MonadBenignIO m
--     , Has ReactorEnv r
--     , MonadReader r m
--     )
--     => (c -> m ())
--     -> WeakObj s
--     -> JE.JSRep
--     -> J.JSString
--     -> (JE.JSRep -> MaybeT IO a)
--     -> (a -> c)
--     -> m ()
-- execRegisterDOMListener executor obj j n goStrict goLazy = void . runMaybeT $ do
--     liftIO $ putStrLn "LOUISDEBUG: execRegisterDOMListener"
--     obj' <- benignDeRefWeakObj obj
--     let mdlRef = sceneRef obj'
--         mdlVar = sceneVar obj'
--     -- Add the handler to the state
--     UnliftIO u <- lift askUnliftIO
--     -- generate a unique id
--     k <- execMkReactId (n NE.:| [])
--     liftIO $ do
--         mdl <- takeMVar mdlVar
--         -- since k is unique, it'll always be a new map item
--         -- update the ioref with the new handler
--         listenerRef <- newIORef mempty
--         cb <- mkEventCallback listenerRef
--         addEventHandler goStrict (u . executor . goLazy) listenerRef
--         -- prepare the updated state,
--         let mdl' = mdl & _plan._domlListeners.at k .~ (Just (cb, listenerRef))
--                 & _plan._finalCleanup %~ (*> removeDomListener j n cb)
--         -- Update the subject
--         atomicWriteIORef mdlRef mdl'
--         putMVar mdlVar mdl'
--         -- now add the domListener to the javascript target
--         addDomListener j n cb

-- addDomListener :: JE.JSRep -> J.JSString -> J.Callback (J.JSVal -> IO ()) -> IO ()
-- addDomListener j n cb = js_addDomListener (JE.toJS j) n (JE.toJS cb)

-- removeDomListener :: JE.JSRep -> J.JSString -> J.Callback (J.JSVal -> IO ()) -> IO ()
-- removeDomListener j n cb = js_removeDomListener (JE.toJS j) n (JE.toJS cb)

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
