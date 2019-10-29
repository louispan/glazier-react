{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Core
    ( CmdReactant
    , Reactant
    , MonadGadget'(..)
    , MonadGadget
    , MonadWidget'
    , MonadWidget
    , logPrefix
    , logJS
    , mkReactId
    , mkObj
    , mkObj2
    , mkLinkedObj
    , mkLinkedObj2
    , readObj
    , unwatchObj
    , model
    , premodel
    , readModel
    , prereadModel
    , noisyMutate
    , quietMutate
    , notifyDirty
    , mkHandler
    , mkHandler'
    , handleSyntheticEvent
    , mkListener
    , listenEventTarget
    , Gizmo
    , txt
    , classNames
    , lf
    , bh
    , displayObj
    ) where

import Control.Also
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Environ
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import qualified Data.JSString as J
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Profunctor.Unsafe
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
import Glazier.React.Core.Internal
import Glazier.React.Gadget
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.Reactant
import Glazier.React.Reactor
import Glazier.React.ReactPath
import qualified JavaScript.Extras as JE
import System.Mem.Weak

------------------------------------------------------
-- Logging
------------------------------------------------------

logPrefix :: MonadGadget' m => m J.JSString
logPrefix = do
    ps <- getReactPath <$> askEnv' @ReactPath
    let xs = L.intersperse "." $ (\(n, i) -> n <> (fromString $ show i)) <$> ps
    pure (foldr (<>) "" xs)

logJS :: (HasCallStack, MonadGadget' m)
    => LogLevel -> IO J.JSString
    -> m ()
logJS lvl msg = withFrozenCallStack $ do
    p <- logPrefix
    n <- untag' @"LogName" <$> askEnv' @LogName
    logLine basicLogCallStackDepth lvl $ (\x -> n <> "<" <> p <> "> " <> x) <$> msg

------------------------------------------------------
-- Basic
------------------------------------------------------

-- | 'mkObj'' that also handles the @a@ for 'Widget's that return an @a@
mkObj2 :: MonadGadget' m => Widget t (Command m) a -> LogName -> t -> m (Either a (Obj t))
mkObj2 wid logname s = do
    x <- devolve wid
    case x of
        Left a -> pure $ Left a
        Right wid' -> Right <$> mkObj wid' logname s

-- | Make an initialized 'Obj' using the given 'Widget' and @s@
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj :: MonadGadget' m => Widget t (Command m) () -> LogName -> t -> m (Obj t)
mkObj wid logname s = do
    s' <- mkModel s
    delegatify $ exec' . MkObj wid logname s'

-- | 'mkLinkedObj'' that also handles the @a@ for 'Widget's that return an @a@
mkLinkedObj2 :: MonadGadget' m => Widget t (Command m) a -> LogName -> Obj t -> m (Either a (Obj t))
mkLinkedObj2 wid logname obj = do
    x <- devolve wid
    case x of
        Left a -> pure $ Left a
        Right wid' -> Right <$> mkLinkedObj wid' logname obj

-- | Make an initialized 'Obj' using the given 'Widget' linked to the data in another 'Obj'
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkLinkedObj :: MonadGadget' m => Widget t (Command m) () -> LogName -> Obj t -> m (Obj t)
mkLinkedObj wid logname (Obj _ _ notifierRef notifierWkRef mdlVar mdlWkVar) =
    delegatify $ exec' . MkObj wid logname (notifierRef, notifierWkRef, mdlVar, mdlWkVar)

-- | Reads from an 'Obj', also registering this as a listener
-- so this will get rerendered whenever the 'Obj' is 'mutate'd.
--
-- EMPTY: This may 'empty' if this MonadGadget has been garbage collected
readObj :: MonadGadget' m => Obj t -> m t
readObj (Obj _ _ notifierRef notifierWkRef mdlVar _) = do
    plnWkRef <- askPlanWeakRef
    plnRef <- guardJustIO $ deRefWeak plnWkRef
    watchModel (plnRef, plnWkRef) (notifierRef, notifierWkRef)
    -- finally we can read the model
    liftIO $ readMVar mdlVar

-- | Reads from an 'Obj', also registering this as a listener
-- so this will get rerendered whenever the 'Obj' is 'mutate'd.
--
-- FAILURES: This may 'empty if this MonadGadget has been garbage collected
unwatchObj :: MonadGadget' m => Obj t -> m ()
unwatchObj (Obj _ _ notifierRef _ _ _) = do
    plnWkRef <- askPlanWeakRef
    plnRef <- guardJustIO $ deRefWeak plnWkRef
    unwatchModel plnRef notifierRef

----------------------------------------------------------------

-- | This is like 'view' but for the cached value in 'AskModel', not 'MonadReader'
-- It acutally uses 'premodel' under the hood and fails with  'empty' instead of
-- returning 'Maybe'.
--
-- FAILURES: This may fail if the @s@ model cannot be obtained (eg. due to 'zoomModel')
-- This may also fail if the @a@ cannot be obtained.
model :: MonadGadget s m => Getting (First a) s a -> m a
model l = guardJustM $ premodel l

-- | This is like 'preview' but for the cached value in  'AskModel', not 'MonadReader'
-- If the lens is a 'Traversal' then @a@ doesn't to be a Monoid,
-- but needs the return type to be @Maybe a@
--
-- FAILURES: This may 'empty' if the @s@ model cannot be obtained (eg. due to 'zoomModel')
premodel :: MonadGadget s m => Getting (First a) s a -> m (Maybe a)
premodel l = do
    (ms, _, _) <- askModelEnviron
    s <- guardJust ms
    pure (getFirst #. foldMapOf l (First #. Just) $ s)


----------------------------------------------------------------

-- | This is like 'model' but reads the latest value from the @IO (Maybe s)@ in 'AskModelEnviron'
--
-- FAILURES: This may 'empty' if the @s@ model cannot be obtained (eg. due to 'zoomModel')
-- This may also 'empty' if the @a@ cannot be obtained.
readModel :: MonadGadget s m => Getting (First a) s a -> m a
readModel l =  guardJustM $ prereadModel l

-- | This is like 'premodel' but reads the latest value from the @IO (Maybe s)@ in 'AskModelEnviron'
--
-- FAILURES: This may 'empty' if the @s@ model cannot be obtained (eg. due to 'zoomModel')
prereadModel :: MonadGadget s m => Getting (First a) s a -> m (Maybe a)
prereadModel l = do
    (_, ms, _) <- askModelEnviron
    s <- guardJustIO ms
    pure (getFirst #. foldMapOf l (First #. Just) $ s)

----------------------------------------------------------------

-- | Quietly mutate the model usin the given 'MaybeT' 'State' monad.
-- Doesn't notify any interested widgets to be rerendered.
--
-- FAILURES: This may 'empty' if the MaybeT State monad fails.
quietMutate :: MonadGadget s m => MaybeT (State s) a -> m a
quietMutate m = do
    (_, _, ModifyModel f) <- askModelEnviron
    guardJustIO $ f m

-- | Noisly mutate the model usin the given 'MaybeT' 'State' monad.
-- Notifies interested widgets to be rerendered.
--
-- FAILURES: This may 'empty' if the MaybeT State monad fails.
noisyMutate :: MonadGadget s m => MaybeT (State s) a -> m a
noisyMutate m = do
    a <- quietMutate m
    notifyDirty
    pure a

-- | Notifys any watchers (from 'readWeakObj')
-- that the model has changed so that the watchers can rerender.
-- Any rerendering is batched and might be be done immediately
notifyDirty :: MonadGadget' m => m ()
notifyDirty = do
    notifierWk <- askNotifierWeakRef
    exec' $ NotifyDirty notifierWk

-- | This convert the input @goStrict@ and @f@ into (preprocess, postprocess).
-- Multiple preprocess must be run for the same event before any of the postprocess,
-- due to the way ghcjs sync and async threads interact with React js.
mkHandler ::
    (NFData a, MonadGadget' m)
    => (J.JSVal -> MaybeT IO a)
    -> (a -> GadgetT m ())
    -> m Handler -- (preprocess, postprocess)
mkHandler goStrict f = do
    plnRef <- askPlanWeakRef
    f' <- codify (runGadgetT . f)
    delegatify $ exec' . MkHandler plnRef goStrict f'

mkHandler' ::
    (NFData a, MonadGadget' m)
    => (SyntheticEvent -> MaybeT IO a)
    -> (a -> GadgetT m ())
    -> m Handler
mkHandler' goStrict goLazy = mkHandler (handleSyntheticEvent goStrict) goLazy

handleSyntheticEvent :: Monad m => (SyntheticEvent -> MaybeT m a) -> (J.JSVal -> MaybeT m a)
handleSyntheticEvent g j = MaybeT (pure $ JE.fromJS j) >>= g

-- | This convert 'Handler' into a ghcjs 'Callback'
mkListener ::
    (MonadGadget' m)
    => Handler
    -> m Listener
mkListener f = do
    plnRef <- askPlanWeakRef
    delegatify $ exec' . MkListener plnRef f

-- | Add a listener with an event target, and automatically removes it on widget destruction
-- This only does something during initialization
listenEventTarget :: (NFData a, MonadGadget' m, IEventTarget j)
    => j -> J.JSString -> (J.JSVal -> MaybeT IO a) -> (a -> GadgetT m ()) -> m (DL.DList (J.JSString, m Handler))
listenEventTarget j n goStrict goLazy = do
        hdl <- mkHandler goStrict goLazy
        cb <- mkListener hdl
        pure [("onConstruct", onConstruct cb ), ("onDestruct", onDestruct cb)]
  where
    onConstruct cb = mkHandler (const $ pure ()) $ const $ do
        liftIO $ addEventListener j n cb

    onDestruct cb = mkHandler (const $ pure ()) $ const $ do
        liftIO $ removeEventListener j n cb

-- | Orphan instance because it requires AsReactant
-- LOUISFIXME: Think about this, use ReaderT (s -> Either e Obj s)?
-- Use a new typeclass?
-- instance (A.AFromJSON m s, AsReactant c, MonadCommand c m, LogLevelEnv m, MonadTrans t, MonadReader (Widget c s s a, J.JSString) (t m), MonadError a (t m)) => A.AFromJSON (t m) (Obj s) where
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

-- | A 'MonadModel' and 'MonadGadget' (but not 'MonadWidget') that may
-- possible fail with 'Alternative'.
type Gizmo s m a = GadgetT (ModelT s m) a

-- | Possibly write some text, otherwise do nothing
txt :: MonadWidget s m => Gizmo s m J.JSString -> m ()
txt m = (`also` pure ()) $ do -- catch failure with `also` so we can continue
            t <- fromModelT . runGadgetT $ m
            textMarkup t

-- | Creates a JSVal for "className" property from a list of (JSString, Bool)
-- Idea from https://github.com/JedWatson/classnames
-- Any 'Alternative' failures to produce 'Bool' is dropped, and does not affect
-- the rest of the list.
classNames :: MonadGadget' m => [(J.JSString, Gizmo s m Bool)] -> Gizmo s m J.JSVal
classNames xs = do
    xs' <- filterM (go . snd) xs
    pure . JE.toJS . J.unwords . fmap fst $ xs'
  where
    go m = m `also` pure False

runProps :: MonadGadget' m
    => [(J.JSString, Gizmo s m J.JSVal)]
    -> ModelT s m [(J.JSString, J.JSVal)]
runProps props = concat <$> traverse f props
  where
    -- emit empty list of a prop fails
    f :: MonadGadget' m => (J.JSString, Gizmo s m J.JSVal) -> ModelT s m [(J.JSString, J.JSVal)]
    f (n, m) = (`also` pure []) $ (\v -> [(n, v)]) <$> runGadgetT m

runGads :: MonadGadget' m
    => [(J.JSString, GadgetT m Handler)]
    -> m [(J.JSString, J.JSVal)]
runGads gads = do
    gads' <- concat <$> traverse f gads -- :: m [(JString, Handler)]
    let gads'' = M.fromListWith (<>) gads' -- combine same keys together
        -- ElementComponent's ref callback is actuall elementRef, so rename ref to elementRef
        gads''' = case M.lookup "ref" gads'' of
                    Nothing -> gads''
                    Just v -> M.insertWith (<>) "elementRef" v gads''
        g = fmap JE.toJS . mkListener -- convert to JS callback
    traverse (traverse g) (M.toList gads''')
  where
    -- emit empty list of a prop fails
    f :: MonadGadget' m => (J.JSString, GadgetT m Handler) -> m [(J.JSString, Handler)]
    f (n, m) = (`also` pure []) $ (\v -> [(n, v)]) <$> runGadgetT m

lf :: (Component j, MonadWidget s m)
    => j -- ^ "input" or a @ReactComponent@
    -> DL.DList (J.JSString, Gizmo s m Handler)
    -> DL.DList (J.JSString, Gizmo s m J.JSVal)
    -> m ()
lf j gads props = do
    modifyEnv' $ nextReactPath (componentName j)
    (props', gads') <- fromModelT $ do
        props' <- runProps (DL.toList props)
        gads' <- runGads (DL.toList gads)
        pure (props', gads')
    let elemMarkup = leafMarkup (JE.toJS j) (DL.fromList (props' <> gads'))
        basicMarkup = leafMarkup (JE.toJS elementComponent)
            (DL.fromList (("elementName", JE.toJS $ componentName j) : props'))
    case (isStringComponent j, gads') of
        (True, []) -> basicMarkup
        _ -> elemMarkup

    leafMarkup (JE.toJS j) (DL.fromList (props' <> gads'))

bh :: (Component j, MonadWidget s m)
    => j-- ^ eg "div" or a @ReactComponent@
    -> DL.DList (J.JSString, Gizmo s m Handler)
    -> DL.DList (J.JSString, Gizmo s m J.JSVal)
    -> m a
    -> m a
bh j gads props child = do
    modifyEnv' $ nextReactPath (componentName j)
    (props', gads') <- fromModelT $ do
        props' <- runProps (DL.toList props)
        gads' <- runGads (DL.toList gads)
        pure (props', gads')
    localEnv' pushReactPath $ do
        let elemMarkup = branchMarkup (JE.toJS j) (DL.fromList (props' <> gads')) child
            basicMarkup = branchMarkup (JE.toJS elementComponent)
                (DL.fromList (("elementName", JE.toJS $ componentName j) : props')) child
        case (isStringComponent j, gads') of
            (True, []) -> basicMarkup
            _ -> elemMarkup

displayObj :: MonadWidget' m => Obj t -> m ()
displayObj (Obj plnRef _ _ _ _ _) = do
    pln <- liftIO $ readIORef plnRef
    let cbs = widgetCallbacks pln
        renderCb = widgetOnRender cbs
        refCb = widgetOnRef cbs
        n = logName pln
    -- These are the callbacks on the 'WidgetComponent'
    -- See jsbits/glazier_react.js
    leafMarkup (JE.toJS widgetComponent)
        [ ("key", JE.toJS $ untag' @"LogName" n)
        , ("render", JE.toJS renderCb)
        , ("ref", JE.toJS refCb)
        ]


