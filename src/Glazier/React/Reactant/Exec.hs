{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE BangPatterns #-}
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

module Glazier.React.Reactant.Exec where

import Control.Also
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Environ
import Control.Monad.IO.Unlift
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
import qualified Data.List as L
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
import Glazier.React.Component
import Glazier.React.Core
import Glazier.React.Core.Internal
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.ReactBatch
import Glazier.React.ReactId.Internal
import Glazier.React.Reactor
import Glazier.React.ReactPath
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO
import System.IO
import System.Mem.AnyStableName
import System.Mem.Weak

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- data ReactantEnv = ReactantEnv
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

type AskLogConfigRef = MonadAsk' (IORef LogConfig)
askLogConfigRef :: AskLogConfigRef m => m (IORef LogConfig)
askLogConfigRef = askEnv' @(IORef LogConfig)

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

type AskNextReactIdRef = MonadAsk' (IORef (Tagged "NextReactId" ReactId))
askNextReactIdRef :: AskNextReactIdRef m => m (IORef (Tagged "NextReactId" ReactId))
askNextReactIdRef = askEnv' @(IORef (Tagged "NextReactId" ReactId))

execMkReactId :: (MonadIO m, AskNextReactIdRef m) => m ReactId
execMkReactId = do
    ref <- askNextReactIdRef
    liftIO $ atomicModifyIORef' ref $ \n ->
        let ReactId i = untag' @"NextReactId" n
        in ( Tagged @"NextReactId" . ReactId $ i + 1, ReactId i)

-----------------------------------------------

type AskDirtyPlan = MonadAsk' (Tagged "DirtyPlan" (IORef (M.Map ReactId (Weak (IORef Plan)))))
askDirtyPlan :: AskDirtyPlan m => m (Tagged "DirtyPlan" (IORef (M.Map ReactId (Weak (IORef Plan)))))
askDirtyPlan = askEnv' @(Tagged "DirtyPlan" (IORef (M.Map ReactId (Weak (IORef Plan)))))

rerenderDirtyPlans :: (MonadAsk' ReactBatch m, AskDirtyPlan m, AlternativeIO m) => m ()
rerenderDirtyPlans = do
    ref <- untag' @"DirtyPlan" <$> askDirtyPlan
    ds <- liftIO $ atomicModifyIORef' ref $ \ds -> (mempty, ds)
    liftIO $ foldMap prerndr ds >>= liftIO -- possibly async GHCJS
    -- by this time, there is a possibilty that shms were removed,
    -- only only batch shms that are still valid
    btch <- askEnv' @ReactBatch
    liftIO $ foldMap (batchWid btch) ds >>= liftIO -- possibly async GHCJS
    -- now run the batch tell react to use the prerendered frames
    liftIO $ runReactBatch btch
  where
    prerndr plnWkRef = (`evalMaybeT` (pure ())) $ do
        plnRef <- guardJustIO $ deRefWeak plnWkRef
        liftIO $ prerender <$> readIORef plnRef
    batchWid btch plnWkRef = (`evalMaybeT` (pure ())) $ do
        plnRef <- guardJustIO $ deRefWeak plnWkRef
        wid <- guardJustIO $ widgetRef <$> readIORef plnRef
        pure $ batchWidgetRerender btch wid

-- | Called after a mutation
markPlanDirty :: (AlternativeIO m, AskDirtyPlan m) => Weak (IORef Plan) -> m ()
markPlanDirty wk = do
    fixme $ liftIO $ putStrLn "LOUISDEBUG: markDirty"
    plnRef <- guardJustIO $ deRefWeak wk
    (oldReq, i) <- liftIO $ atomicModifyIORef' plnRef $ \pln ->
        let (oldReq, pln') = (pln & _rerenderRequired <<.~ RerenderNotRequired)
        in (pln', (oldReq, planId pln))
    case oldReq of
        RerenderRequired -> pure () -- we have scheduled already
        RerenderNotRequired -> do
            -- Add plan to pending list for worker thread
            dirtRef <- untag' @"DirtyPlan" <$> askDirtyPlan
            liftIO $ atomicModifyIORef_' dirtRef $ M.insert i wk




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
--     , Has ReactantEnv r
--     , MonadReader r m
--     , AsReactant c
--     , Typeable s
--     )
--     => (c -> m ()) -> Widget c s () -> NE.NonEmpty J.JSString -> s -> JE.JSRep -> m (J.Export (Obj s))
-- startWidget executor wid logname s root = execMkObj executor wid logname s >>= startObj root

-- -- | Returns commands that need to be processed last
-- execReactantCmd ::
--     ( MonadUnliftIO m
--     , MonadBenignIO m
--     , MonadReader r m
--     , AsReactant c
--     , Has ReactantEnv r
--     )
--     => (c -> m ()) -> ReactantCmd c -> m [c]
-- execReactantCmd executor c = case c of
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

mkObj ::
    ( MonadIO m
    , MonadUnliftIO m
    , AskLogConfigRef m
    , AskNextReactIdRef m
    , CmdReactant c
    )
    => (c -> m ())
    -> Widget s c ()
    -> LogName
    -> (IORef Notifier, Weak (IORef Notifier), MVar s, Weak (MVar s))
    -> m (Obj s)
mkObj executor wid logName' (notifierRef_, notifierWkRef, mdlVar_, mdlWkVar) = do
    UnliftIO u <- askUnliftIO
    fixme $ liftIO $ putStrLn "LOUISDEBUG: execMkObj"
    (logLevel', logDepth') <- getLogConfig logName'
    i <- execMkReactId
    o <- liftIO $ JO.create
    plnRef_ <- liftIO $ newIORef $ Plan
        i
        logName'
        logLevel'
        logDepth'
        o
        Nothing -- widgetRef
        J.nullRef -- prerendered, null for now
        mempty -- prerender
        RerenderRequired -- prerendered is null
        mempty -- notifiers
        mempty -- createdHandlers
        mempty -- createdCallbacks
        (WidgetCallbacks
            (J.Callback J.nullRef)
            (J.Callback J.nullRef))

    -- Create automatic garbage collection of the callbacks
    -- that will run when the Obj is garbage collected.
    plnWkRef <- liftIO $ mkWeakIORef plnRef_ $ do
        fixme $ putStrLn "LOUISDEBUG: release plnRef"
        -- references for finalizers do not keep reference alive
        pln <- readIORef plnRef_
        (`evalMaybeT` ()) $ do
            notifierRef <- guardJustIO $ deRefWeak notifierWkRef
            liftIO $ atomicModifyIORef_' notifierRef (_watchers.at i .~ Nothing)
        releasePlanCallbacks pln

    -- Now we have enough to run the widget
    let wid' = do
            -- only run if 'RerenderRequired'
            plnRef <- guardJustIO $ deRefWeak plnWkRef
            req <- liftIO $ atomicModifyIORef' plnRef $ \pln ->
                swap (pln & _rerenderRequired <<.~ RerenderNotRequired)

            case req of
                RerenderNotRequired -> pure ()
                RerenderRequired -> do
                     -- protect against a 'finish'ed widget,
                     -- so we can get the partial markup.
                    wid `also` pure ()
                    -- get the window out of wid and set the rendering function
                    -- the window cannot be obtained from execStateT because it
                    -- will return in the partial result due to StateT instance of 'codify'
                    -- So use 'SetPrerendered' to store the final window.
                    ml <- askMarkup
                    setPrerendered plnWkRef ml

        -- mkRerenderCmds :: (c -> m ()) -> IO (DL.DList c)
        mkRerenderCmds = (`evalMaybeT` mempty) $ do
            -- get the latest state from the weak ref
            mdlVar' <- guardJustIO $ deRefWeak mdlWkVar
            mdl <- liftIO $ readMVar mdlVar'
            plnRef' <- guardJustIO $ deRefWeak plnWkRef
            o' <- liftIO $ scratch <$> readIORef plnRef'
            -- then get the latest markup using the state
            liftIO . execProgramT'
                . (`evalStateT` mempty) -- markup
                . (`evalStateT` (ReactPath (Nothing, [])))
                . evalContT
                . (`evalMaybeT` ())
                . (`runReaderT` plnWkRef)
                . (`runReaderT` notifierWkRef)
                . (`runReaderT` Tagged @"Scratch" o')
                . runReactor
                . (`runModelT` (Just mdl, readModelWith mdlWkVar, modelStateWith mdlWkVar))
                $ wid'
        -- prerndr :: (c -> m ()) -> IO ()
        prerndr = do
            c <- (commands . DL.toList) <$> mkRerenderCmds
            u $ executor c

    renderCb <- liftIO . J.syncCallback' $ onRenderCb plnWkRef
    refCb <- liftIO . J.syncCallback1 J.ContinueAsync $ onRefCb plnWkRef

    -- update the plan to include the real WidgetCallbacks and prerender functon
    liftIO $ atomicModifyIORef_' plnRef_ $ \pln ->
        (pln
            { widgetCallbacks = WidgetCallbacks renderCb refCb
            , prerender = prerndr
            })

    -- link the plan ot the model so it gets notified of mutations
    watchModel (plnRef_, plnWkRef) (notifierRef_, notifierWkRef)

    -- run the prerender function for the first time
    -- This will also initialize the widget, and set the rendered frame in the Plan
    liftIO prerndr

    -- We do not want explicitly tell React to show the prendered frame right now.
    -- This is the responsiblity of the caller of MkObj

    -- return the obj created
    pure $ Obj plnRef_ plnWkRef notifierRef_ notifierWkRef mdlVar_ mdlWkVar

  where
    setPrerendered :: AlternativeIO m => Weak (IORef Plan) -> DL.DList ReactMarkup -> m ()
    setPrerendered plnWkRef mrkup = do
        frame <- liftIO $ JE.toJS <$> toElement mrkup
        fixme $ liftIO $ putStrLn "LOUISDEBUG: execSetPrerendered"
        plnRef <- guardJustIO $ deRefWeak plnWkRef
        -- replace the prerendered frame
        liftIO $ atomicModifyIORef_' plnRef $ (_prerendered .~ frame)

    onRenderCb :: Weak (IORef Plan) -> IO J.JSVal
    onRenderCb wk = (`evalMaybeT` J.nullRef) $ do
        plnRef <- guardJustIO $ deRefWeak wk
        liftIO $ do
            pln <- readIORef plnRef
            -- prerendered is guaranteed to be ready because the widget
            -- should have been initialized before being mounted
            pure $ prerendered pln

    onRefCb :: Weak (IORef Plan) -> J.JSVal -> IO ()
    onRefCb wk j = (`evalMaybeT` ()) $ do
        plnRef <- guardJustIO $ deRefWeak wk
        liftIO $ atomicModifyIORef_' plnRef (_widgetRef .~ JE.fromJS j)

-- execMutate ::
--     (AlternativeIO m, AskDirtyPlan m)
--     => (c -> m ())
--     -> Weak (MVar s)
--     -> State s c
--     -> m ()
-- execMutate executor mdlWk tick = do
--     liftIO $ putStrLn $ "LOUISDEBUG: execMutate"
--     mdlVar <- guardJustIO $ deRefWeak mdlWk
--     c <- liftIO $ modifyMVar mdlVar (pure . swap . runState tick)
--     executor c

execNotifyDirty ::
    (AlternativeIO m, AskDirtyPlan m)
    => Weak (IORef Notifier)
    -> m ()
execNotifyDirty notifierWkRef = do
    liftIO $ putStrLn $ "LOUISDEBUG: execNotifyDirty"
    notifierRef <- guardJustIO $ deRefWeak notifierWkRef
    ws <- liftIO $ watchers <$> readIORef notifierRef
    foldr (\wk b -> markPlanDirty wk *> b) (pure ()) ws

execMkHandler :: (NFData a, AlternativeIO m, MonadUnliftIO m)
        => (c -> m ())
        -> Weak (IORef Plan)
        -> (J.JSVal -> MaybeT IO a)
        -> (a -> c)
        -- (preprocess, postprocess)
        -> MaybeT m Handler
execMkHandler executor plnkWk goStrict goLazy = do
    -- 'makeStableName' might return different names if unevaluated
    -- so use bang patterns to help prevent that.
    UnliftIO u <- lift $ askUnliftIO
    let !goStrict' = goStrict
        !goLazy' = u . executor . goLazy
    k <- liftIO $ makeAnyStableName (goStrict', goLazy')

    -- check to see if this already has been created
    plnRef <- guardJustIO $ deRefWeak plnkWk
    hs <- liftIO $ handlers <$> readIORef plnRef
    case L.find ((== k) . fst) hs of
        Just (_, v) -> pure v
        Nothing -> do
            (x, y) <- liftIO $ mkEventProcessor goStrict'
            let f = (x, (`evalMaybeT` ()) (y >>= lift . goLazy'))
            liftIO $ atomicModifyIORef_ plnRef (_handlers %~ ((k, f) :))
            pure f

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
        postprocess = guardJustIO $ atomically $ tryReadTQueue c
    pure (preprocess, postprocess)


execMkListener :: (MonadIO m)
        => Weak (IORef Plan)
        -> Handler
        -> MaybeT m (J.Callback (J.JSVal -> IO ()))
execMkListener plnkWk (g, h) = do
    -- 'makeStableName' might return different names if unevaluated
    -- so use bang patterns to help prevent that.
    let !g' = g
        !h' = h
        !hdl' = (g', h')
    k <- liftIO $ makeAnyStableName hdl'

    -- check to see if this already has been created
    plnRef <- guardJustIO $ deRefWeak plnkWk
    cs <- liftIO $ listeners <$> readIORef plnRef
    case L.find ((== k) . fst) cs of
        Just (_, v) -> pure v
        Nothing -> do
            f <- liftIO $ J.asyncCallback1 (\j -> g' j *> h')
            liftIO $ atomicModifyIORef_ plnRef (_listeners %~ ((k, f) :))
            pure f

#ifdef __GHCJS__

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

js_logInfo :: J.JSString -> IO ()
js_logInfo = putStrLn . J.unpack

js_logWarn :: J.JSString -> IO ()
js_logWarn = hPutStrLn stderr . J.unpack

js_logError :: J.JSString -> IO ()
js_logError = hPutStrLn stderr . J.unpack

#endif