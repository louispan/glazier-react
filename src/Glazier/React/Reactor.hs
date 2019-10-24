{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Reactor
    ( CmdReactor
    , MonadGadget
    , MonadWidget
    , Reactor
    , logPrefix
    , logJS
    , mkReactId
    , mkObj
    , mkObj2
    , mkLinkedObj
    , mkLinkedObj2
    , readObj
    , unwatchObj
    , noisyMutate
    , quietMutate
    , notifyDirty
    , mkHandler
    , mkHandler'
    , handleSyntheticEvent
    , mkListener
    , listenEventTarget
    , runGadget
    , Prop
    , txt
    , classNames
    , lf
    , bh
    , displayObj
    ) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import qualified Data.JSString as J
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import Data.Tagged.Extras
import GHC.Stack
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.DOM.Event
import Glazier.DOM.EventTarget
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Component
import Glazier.React.Markup
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.Reactor.Internal
import Glazier.React.ReactPath
import Glazier.React.Widget
import qualified JavaScript.Extras as JE
import System.Mem.Weak

logPrefix :: MonadGadget s m => m J.JSString
logPrefix = do
    ps <- getReactPath <$> askReactPath
    let xs = L.intersperse "." $ (\(n, i) -> n <> (fromString $ show i)) <$> ps
    pure (foldr (<>) "" xs)

logJS :: (HasCallStack, MonadGadget s m)
    => LogLevel -> IO J.JSString
    -> m ()
logJS lvl msg = withFrozenCallStack $ do
    p <- logPrefix
    n <- untag' @"LogName" <$> askLogName
    logLine basicLogCallStackDepth lvl $ (\x -> n <> "<" <> p <> "> " <> x) <$> msg

------------------------------------------------------
-- Basic
------------------------------------------------------

-- | 'mkObj'' that also handles the @a@ for 'Widget's that return an @a@
mkObj2 :: MonadGadget s m => Widget t (Command m) a -> LogName -> t -> m (Either a (Obj t))
mkObj2 wid logname s = do
    x <- delegating wid
    case x of
        Left a -> pure $ Left a
        Right wid' -> Right <$> mkObj wid' logname s

-- | Make an initialized 'Obj' using the given 'Widget' and @s@
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj :: MonadGadget s m => Widget t (Command m) () -> LogName -> t -> m (Obj t)
mkObj wid logname s = do
    s' <- mkModel s
    delegatify $ exec' . MkObj wid logname s'

-- | 'mkLinkedObj'' that also handles the @a@ for 'Widget's that return an @a@
mkLinkedObj2 :: MonadGadget s m => Widget t (Command m) a -> LogName -> Obj t -> m (Either a (Obj t))
mkLinkedObj2 wid logname obj = do
    x <- delegating wid
    case x of
        Left a -> pure $ Left a
        Right wid' -> Right <$> mkLinkedObj wid' logname obj

-- | Make an initialized 'Obj' using the given 'Widget' linked to the data in another 'Obj'
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkLinkedObj :: MonadGadget s m => Widget t (Command m) () -> LogName -> Obj t -> m (Obj t)
mkLinkedObj wid logname (Obj _ _ notifierRef notifierWkRef mdlVar mdlWkVar) =
    delegatify $ exec' . MkObj wid logname (notifierRef, notifierWkRef, mdlVar, mdlWkVar)

-- | Reads from an 'Obj', also registering this as a listener
-- so this will get rerendered whenever the 'Obj' is 'mutate'd.
readObj :: MonadGadget s m => Obj t -> m t
readObj (Obj _ _ notifierRef notifierWkRef mdlVar _) = do
    plnWkRef <- askPlanWeakRef
    plnRef <- fromJustIO $ deRefWeak plnWkRef
    watchModel (plnRef, plnWkRef) (notifierRef, notifierWkRef)
    -- finally we can read the model
    liftIO $ readMVar mdlVar

-- | Reads from an 'Obj', also registering this as a listener
-- so this will get rerendered whenever the 'Obj' is 'mutate'd.
unwatchObj :: MonadGadget s m => Obj t -> m ()
unwatchObj (Obj _ _ notifierRef _ _ _) = do
    plnWkRef <- askPlanWeakRef
    plnRef <- fromJustIO $ deRefWeak plnWkRef
    unwatchModel plnRef notifierRef

-- | Mutates the Model for the current widget.
-- Doesn't automatically flags the widget as dirty
noisyMutate :: MonadGadget s m => StateT s IO a -> m a
noisyMutate m = do
    a <- quietMutate m
    notifyDirty
    pure a

-- | Mutates the Model for the current widget.
-- Doesn't automatically flags the widget as dirty
quietMutate :: MonadGadget s m => StateT s IO a -> m a
quietMutate m = do
    mdlWk <- askModelWeakVar
    delegatify $ \f -> exec' $ Mutate mdlWk (f <$> m)

-- | Notifys any watchers (from 'readWeakObj')
-- that the model has changed so that the watchers can rerender.
-- Any rerendering is batched and might be be done immediately
notifyDirty :: MonadGadget s m => m ()
notifyDirty = do
    notifierWk <- askNotifierWeakRef
    exec' $ NotifyDirty notifierWk

-- | This convert the input @goStrict@ and @f@ into (preprocess, postprocess).
-- Multiple preprocess must be run for the same event before any of the postprocess,
-- due to the way ghcjs sync and async threads interact with React js.
mkHandler ::
    (NFData a, MonadGadget s m)
    => (J.JSVal -> MaybeT IO a)
    -> (a -> GadgetT m ())
    -> m Handler -- (preprocess, postprocess)
mkHandler goStrict f = do
    plnRef <- askPlanWeakRef
    f' <- codify (runGadgetT . f)
    delegatify $ exec' . MkHandler plnRef goStrict f'

mkHandler' ::
    (NFData a, MonadGadget s m)
    => (SyntheticEvent -> MaybeT IO a)
    -> (a -> GadgetT m ())
    -> m Handler
mkHandler' goStrict goLazy = mkHandler (handleSyntheticEvent goStrict) goLazy

handleSyntheticEvent :: Monad m => (SyntheticEvent -> MaybeT m a) -> (J.JSVal -> MaybeT m a)
handleSyntheticEvent g j = MaybeT (pure $ JE.fromJS j) >>= g

-- | This convert 'Handler' into a ghcjs 'Callback'
mkListener ::
    (MonadGadget s m)
    => Handler
    -> m Listener
mkListener f = do
    plnRef <- askPlanWeakRef
    delegatify $ exec' . MkListener plnRef f

-- | Add a listener with an event target, and automatically removes it on widget destruction
-- This only does something during initialization
listenEventTarget
 :: (NFData a, MonadWidget s m, IEventTarget j)
    => j -> J.JSString -> (J.JSVal -> MaybeT IO a) -> (a -> GadgetT m ()) -> m ()
listenEventTarget j n goStrict goLazy =
    initConstructor $ do
        hdl <- mkHandler goStrict goLazy
        cb <- mkListener hdl
        liftIO $ addEventListener j n cb
        initDestructor $ liftIO $ removeEventListener j n cb


-- | Run a gadget on an @Obj t@
runGadget :: MonadGadget s m => Obj s -> GadgetT m a -> m a
runGadget (Obj plnRef plnWkRef _ notifierWkRef mdlVar mdlWkVar) m = do
    mdl <- liftIO $ readMVar mdlVar
    sch <- liftIO $ scratch <$> readIORef plnRef
    rp <- askReactPath

    let f = localReactPath (const rp)
            . localPlanWeakRef (const plnWkRef)
            . localNotifierWeakRef (const notifierWkRef)
            . localScratch (const sch)
            . localModel (const mdl)
            . localModelWeakVar (const mdlWkVar)
    f (runGadgetT m)

-- | Orphan instance because it requires AsReactor
-- LOUISFIXME: Think about this, use ReaderT (s -> Either e Obj s)?
-- Use a new typeclass?
-- instance (A.AFromJSON m s, AsReactor c, MonadCommand c m, LogLevelEnv m, MonadTrans t, MonadReader (Widget c s s a, J.JSString) (t m), MonadError a (t m)) => A.AFromJSON (t m) (Obj s) where
--     aparseJSON v = do
--         ms <- fmap lift (A.aparseJSON v)
--         let meobj = do
--                 (wid, logname) <- ask
--                 s <- ms
--                 e <- lift $ mkObj wid logname s
--                 case e of
--                     Left e' -> throwError e'
--                     Right obj -> pure obj
--         pure meobj


----------------------------------------------------------------------------------

-- -- | This orphan instance allows using "blah" is a prop in 'txt', 'lf', and 'bh'
-- -- when using @OverloadedString@ with @ExtendedDefaultRules@
-- instance (Applicative m, IsString a) => IsString (Prop s m a) where
--     fromString = pure . Just . fromString
-- wackWidget :: (MonadGadget s m, MonadUnliftWidget s m) => m a -> Obj s -> m a
-- wackWidget m o = do
--     u <- askUnliftWidget
--     runWidget (unliftWidget u m) o


type Prop m a = GadgetT m (Maybe a)

-- | Possibly write some text
txt :: MonadWidget s m => Prop m J.JSString -> m ()
txt m = do
    t <- runGadgetT m
    maybe (pure ()) textMarkup t

-- | Creates a JSVal for "className" property from a list of (JSString, Bool)
-- Idea from https://github.com/JedWatson/classnames
classNames :: Monad m => [(J.JSString, Prop m Bool)] -> Prop m J.JSVal
classNames xs = do
    xs' <- filterM (fmap ok . snd) xs
    pure . Just . JE.toJS . J.unwords . fmap fst $ xs'
  where
    ok (Just True) = True
    ok _ = False

runProps :: Monad m
    => [(J.JSString, Prop m J.JSVal)]
    -> m [(J.JSString, J.JSVal)]
runProps props = do
    let f = fmap (fromMaybe J.nullRef) . runGadgetT
    traverse (traverse f) props

runGads :: (MonadGadget s m)
    => [(J.JSString, m Handler)]
    -> m [(J.JSString, J.JSVal)]
runGads gads = do
    gads' <- traverse sequenceA gads -- :: m [(JString, Handler)]
    let gads'' = M.toList $ M.fromListWith (<>) gads' -- combine same keys together
        f = fmap JE.toJS . mkListener -- convert to JS callback
    traverse (traverse f) gads''

lf :: (Component j, MonadWidget s m)
    => j -- ^ "input" or a @ReactComponent@
    -> DL.DList (J.JSString, m Handler)
    -> DL.DList (J.JSString, Prop m J.JSVal)
    -> m ()
lf j gads props = do
    props' <- runProps (DL.toList props)
    putNextReactPath (componentName j)
    gads' <- runGads (DL.toList gads)
    leafMarkup (JE.toJS j) (DL.fromList (props' <> gads'))

bh :: (Component j, MonadWidget s m)
    => j-- ^ eg "div" or a @ReactComponent@
    -> DL.DList (J.JSString, m Handler)
    -> DL.DList (J.JSString, Prop m J.JSVal)
    -> m a
    -> m a
bh j gads props child = do
    props' <- runProps (DL.toList props)
    putNextReactPath (componentName j)
    gads' <- runGads (DL.toList gads)
    putPushReactPath
    a <- branchMarkup (JE.toJS j) (DL.fromList (props' <> gads'))
        child
    putPopReactPath
    pure a

displayObj :: MonadWidget s m => Obj t -> m ()
displayObj (Obj plnRef _ _ _ _ _) = do
    pln <- liftIO $ readIORef plnRef
    let cbs = widgetCallbacks pln
        renderCb = widgetOnRender cbs
        refCb = widgetOnRef cbs
        renderedCb = widgetOnRendered cbs
        n = logName pln
    -- These are the callbacks on the 'WidgetComponent'
    -- See jsbits/glazier_react.js
    leafMarkup (JE.toJS widgetComponent)
        [ ("key", JE.toJS $ untag' @"LogName" n)
        , ("render", JE.toJS renderCb)
        , ("ref", JE.toJS refCb)
        , ("rendered", JE.toJS renderedCb)
        ]


