{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Core
    ( CmdReactant
    , Reactant
    , MonadReactant
    , MonadGadget'(..)
    , MonadGadget
    , MonadWidget'
    , MonadWidget
    , logPrefix
    , logJS
    , mkReactId
    , onDestruct
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
    , txt
    , cleanWidget
    , classNames
    , lf
    , bh
    , displayObj
    , mkModelRef
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.IORef
import qualified Data.JSString as J
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.String
import Data.Tagged.Extras
import GHC.Stack
import Glazier.Command
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Component
import Glazier.React.Core.Internal
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.Reactant
import Glazier.React.Reactor
import Glazier.React.ReactPath
import Glazier.React.Synthetic
import JS.Data
import JS.DOM.EventTarget
import System.Mem.Weak

default (JSString)

------------------------------------------------------
-- Logging
------------------------------------------------------

logPrefix :: MonadGadget' m => m JSString
logPrefix = do
    ps <- getReactPath <$> getEnv' @ReactPath
    let xs = L.intersperse "." $ (\(n, i) -> n <> (fromString $ show i)) <$> ps
    pure (foldr (<>) "" xs)

logJS :: (HasCallStack, MonadGadget' m)
    => LogLevel -> IO JSString
    -> m ()
logJS lvl msg = withFrozenCallStack $ do
    p <- logPrefix
    n <- untag' @"LogName" <$> askEnv' @LogName
    logLine basicLogCallStackDepth lvl $ (\x -> n <> "<" <> p <> "> " <> x) <$> msg

------------------------------------------------------
-- Basic
------------------------------------------------------

-- | Register the given monad to be evaluated at destruction time of the PlanRef of this widget.
-- This action is done every time, so if want to register something once, only call this once
onDestruct :: MonadGadget' m => m () -> m ()
onDestruct m = do
    plnWkRef <- askPlanWeakRef
    c <- codify' m
    exec' $ RegisterDestructor plnWkRef c

-- | 'mkObj'' that also handles the @a@ for 'Widget's that return an @a@
mkObj2 :: (MonadIO m, MonadCommand m, CmdReactant (Command m)) => Widget t (Command m) a -> LogName -> t -> m (Either a (Obj t))
mkObj2 wid logname s = do
    x <- devolve wid
    case x of
        Left a -> pure $ Left a
        Right wid' -> Right <$> mkObj wid' logname s

-- | Make an initialized 'Obj' using the given 'Widget' and @s@
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkObj :: MonadReactant m => Widget t (Command m) () -> LogName -> t -> m (Obj t)
mkObj wid logname s = do
    s' <- mkModelRef s
    delegatify $ exec' . MkObj wid logname s'

-- | 'mkLinkedObj'' that also handles the @a@ for 'Widget's that return an @a@
mkLinkedObj2 :: MonadReactant m => Widget t (Command m) a -> LogName -> Obj t -> m (Either a (Obj t))
mkLinkedObj2 wid logname obj = do
    x <- devolve wid
    case x of
        Left a -> pure $ Left a
        Right wid' -> Right <$> mkLinkedObj wid' logname obj

-- | Make an initialized 'Obj' using the given 'Widget' linked to the data in another 'Obj'
-- Unlike 'unliftMkObj', this version doesn't required 'MonadUnliftWidget' so @m@ can be any transformer stack.
mkLinkedObj :: MonadReactant m => Widget t (Command m) () -> LogName -> Obj t -> m (Obj t)
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
    watchModelRef (plnRef, plnWkRef) (notifierRef, notifierWkRef)
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
    unwatchModelRef plnRef notifierRef

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
    (ms, _, _) <- askModelEnv
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
    (_, ms, _) <- askModelEnv
    s <- guardJustIO ms
    pure (getFirst #. foldMapOf l (First #. Just) $ s)

----------------------------------------------------------------

-- | Quietly mutate the model usin the given 'MaybeT' 'State' monad.
-- Doesn't notify any interested widgets to be rerendered.
--
-- FAILURES: This may 'empty' if the MaybeT State monad fails.
quietMutate :: MonadGadget s m => MaybeT (State s) a -> m a
quietMutate m = do
    (_, _, ModifyModel f) <- askModelEnv
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
    => (JSVal -> MaybeT IO a)
    -> (a -> m ())
    -> m Handler -- (preprocess, postprocess)
mkHandler goStrict f = do
    plnWkRef <- askPlanWeakRef
    f' <- codify f
    a <- delegatify $ exec' . MkHandler plnWkRef goStrict f'
    pure a

mkHandler' ::
    (NFData a, MonadGadget' m)
    => (SyntheticEvent -> MaybeT IO a)
    -> (a -> m ())
    -> m Handler
mkHandler' goStrict goLazy = mkHandler (handleSyntheticEvent goStrict) goLazy

handleSyntheticEvent :: Monad m => (SyntheticEvent -> MaybeT m a) -> (JSVal -> MaybeT m a)
handleSyntheticEvent g j = MaybeT (pure $ fromJS j) >>= g

-- | Add a listener with an event target, and automatically removes it on widget destruction
listenEventTarget :: (NFData a, MonadWidget' m, IEventTarget j)
    => j -> JSString -> (JSVal -> MaybeT IO a) -> (a -> m ()) -> m ()
listenEventTarget j n goStrict goLazy = do
    hdl <- mkHandler goStrict goLazy
    cb <- mkListener hdl
    liftIO $ addEventListener j n cb
    onDestruct $ liftIO $ removeEventListener j n cb

-- | Orphan instance because it requires AsReactant
-- LOUISFIXME: Think about this, use ReaderT (s -> Either e Obj s)?
-- Use a new typeclass?
-- instance (A.AFromJSON m s, AsReactant c, MonadCommand c m, LogLevelEnv m, MonadTrans t, MonadReader (Widget c s s a, JSString) (t m), MonadError a (t m)) => A.AFromJSON (t m) (Obj s) where
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

-- | Write some text
txt :: MonadWidget s m => ModelT s m JSString -> m ()
txt m = do -- catch failure with `<|>` so we can continue
    t <- fromModelT m
    textMarkup t

-- | Creates a JSVal for "className" property from a list of (JSString, Bool)
-- Idea from https://github.com/JedWatson/classnames
-- Any 'Alternative' failures to produce 'Bool' is dropped, and does not affect
-- the rest of the list.
classNames :: MonadWidget' m => [(JSString, ModelT s m Bool)] -> ModelT s m JSVal
classNames xs = do
    xs' <- filterM (go . snd) xs
    pure . toJS . J.unwords . fmap fst $ xs'
  where
    go m = ((fromMaybe False) <$> (dischargeHead $ cleanWidget m)) <|> pure False

-- | markup a leaf html element, given a DList of @m@ that produces Handlers
-- and property values.
-- If a given @m@ makes multiple Handler/JSVal, only the first one is used.
-- If a given @m@ fails to make anything, then it is safely skipped and the
-- following @m@ in the 'DList' evaluated.
lf :: (Component j, MonadWidget s m)
    => j -- ^ "input" or a @ReactComponent@
    -> DL.DList (JSString, ModelT s m Handler)
    -> DL.DList (JSString, ModelT s m JSVal)
    -> m ()
lf j gads props = do
    modifyEnv' $ nextReactPath (componentName j)
    (props', gads') <- fromModelT $ do
        props' <- sequenceProps $ DL.toList props
        gads' <- sequenceGadgets $ DL.toList gads
        pure (props', gads')

    let origMarkup = leafMarkup (toJS j) (DL.fromList (props' <> gads'))
        elemMarkup = leafMarkup (toJS elementComponent)
            (DL.fromList $ ("elementName", toJS $ componentName j) : (props' <> gads'))
    case (isStringComponent j, gads') of
        (True, []) -> do
            origMarkup
        (True, _) -> do
            elemMarkup
        _ -> do
            origMarkup

-- | markup a branch html element, given a DList of @m@ that produces Handlers
-- and property values, and the @m@ child node.
-- If a given @m@ makes multiple Handler/JSVal, only the first one is used.
-- If a given @m@ fails to make anything, then it is safely skipped and the
-- following @m@ in the 'DList' evaluated.
bh :: (Component j, MonadWidget s m)
    => j-- ^ eg "div" or a @ReactComponent@
    -> DL.DList (JSString, ModelT s m Handler)
    -> DL.DList (JSString, ModelT s m JSVal)
    -> m a
    -> m a
bh j gads props child = do
    modifyEnv' $ nextReactPath (componentName j)
    rp <- getEnv' @ReactPath
    (props', gads') <- fromModelT $ do
        props' <- sequenceProps $ DL.toList props
        gads' <- sequenceGadgets $ DL.toList gads
        pure (props', gads')

    modifyEnv' @ReactPath (const $ pushReactPath rp)
    -- discharge child with noop (no extra markup effects)
    -- to get a child that only fires once
    -- for its markup effects
    -- FIXME: This is wrong too! as discharge doesn't fully run
    -- the commands
    let child' = discharge child (const $ pure ())
        origMarkup = branchMarkup (toJS j) (DL.fromList (props' <> gads'))
            child'
        elemMarkup = branchMarkup (toJS elementComponent)
            (DL.fromList $ ("elementName", toJS $ componentName j) : (props' <> gads'))
            child'
    case (isStringComponent j, gads') of
        (True, []) -> origMarkup
        (True, _) -> elemMarkup
        _ -> origMarkup
    putEnv' @ReactPath rp
    -- now actually use child's events (but not markup)
    a <- cleanWidget $ child
    pure a

displayObj :: (MonadIO m, MonadPut' Markup m) => Obj t -> m ()
displayObj (Obj plnRef _ _ _ _ _) = do
    pln <- liftIO $ readIORef plnRef
    let cbs = widgetCallbacks pln
        renderCb = widgetOnRender cbs
        refCb = widgetOnRef cbs
        n = logName pln
    -- These are the callbacks on the 'WidgetComponent'
    -- See jsbits/glazier_react.js
    leafMarkup (toJS widgetComponent)
        [ ("key", toJS $ untag' @"LogName" n)
        , ("render", toJS renderCb)
        , ("ref", toJS refCb)
        ]


