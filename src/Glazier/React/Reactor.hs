{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Reactor
    ( AsReactor
    , MonadReactor
    , ReactorCmd(..)
    , ModelState
    , evalIO
    , evalIO_
    , evalIOThen
    , mkReactId
    , setRender
    , mkObj
    , mkObj'
    , withMkObj
    , getModel
    , getElementalRef
    , rerender
    , mutate
    , mutateThen
    , domTrigger
    , domTrigger_
    , trigger
    , trigger_
    , onMounted
    , onRendered
    , onNextRendered
    , onMutated
    , onMutated'
    ) where

import Control.Also
import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.EventTarget
import Glazier.React.Notice
import Glazier.React.ReactId
import Glazier.React.ReadIORef
import Glazier.React.Obj
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE
-----------------------------------------------------------------

type AsReactor cmd =
    ( AsFacet [cmd] cmd -- implicity required by 'MonadCodify'
    , AsFacet (ReactorCmd cmd) cmd
    )

type MonadReactor p s cmd m =
    ( AsReactor cmd
    , MonadReader (Entity p s) m
    , MonadCommand cmd m
    , MonadState J.JSString m
    )

type ModelState s = StateT s ReadIORef

-- | NB. 'ReactorCmd' is not a functor because of the @Widget cmd@ in 'MkObj'
data ReactorCmd cmd where
    -- run arbitrary IO, should only be used for debugging
    -- FIXME: Rename to RunIO
    EvalIO :: IO cmd -> ReactorCmd cmd
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> cmd) -> ReactorCmd cmd
    -- | the the rendering function in a Obj, replace any existing render callback
    SetRender :: WeakObj s -> Window s () -> ReactorCmd cmd
    -- | Make a fully initialized object from a widget and model
    MkObj :: Widget cmd s s () -> s -> (Obj s -> cmd) -> ReactorCmd cmd
    -- | Get the model
    GetModel :: WeakObj s -> (s -> cmd) -> ReactorCmd cmd
    -- Get the event target
    -- If a "ref" callback to update 'elementalRef' has not been added;
    -- then add it, rerender, then return the EventTarget.
    GetElementalRef ::
        WeakObj s
        -> ReactId
        -> (EventTarget -> cmd)
        -> ReactorCmd cmd
    -- | Rerender a ShimComponent using the given state.
    Rerender :: WeakObj s -> ReactorCmd cmd
    -- | Private: Renders the object (will only do something first time after Rerender)
    DoRerender :: WeakObj s -> ReactorCmd cmd
    -- | Update and rerender.
    Mutate :: J.JSString -> ReactId -> WeakObj s -> ModelState s cmd -> ReactorCmd cmd
    -- | Private: Calls the model mutatedListener (will only do something first time after Mutate)
    NotifyMutated :: ReactId -> WeakObj s -> ReactorCmd cmd
    -- | Private: Resets the mutated state back to NotMutated
    ResetMutation :: WeakObj s -> ReactorCmd cmd
    -- | Create and register a dom callback
    RegisterDOMListener :: NFData a
        => WeakObj s
        -> JE.JSRep
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ReactorCmd cmd
    -- | Create and register a react callback
    -- If the callback is for "ref", then an listener to update 'elementalRef' for 'GetEventTarget'
    -- will automatically be added just before the listener in 'RegisterReactListener'.
    RegisterReactListener :: NFData a
        => WeakObj s
        -> ReactId
        -> J.JSString
        -> (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ReactorCmd cmd
    -- | Create and register a callback for the mounted event
    RegisterMountedListener ::
        WeakObj s
        -> cmd
        -> ReactorCmd cmd
    -- | Create and register a callback for the rendered event
    RegisterRenderedListener ::
        WeakObj s
        -> cmd
        -> ReactorCmd cmd
    -- | Create and register a callback for the rendered event
    RegisterNextRenderedListener ::
        WeakObj s
        -> cmd
        -> ReactorCmd cmd
    -- | Create and register a callback for the state updated event
    RegisterMutatedListener ::
        WeakObj s
        -> (ReactId -> cmd)
        -> ReactorCmd cmd

-- FIXME: can show ReactId and caption
instance Show (ReactorCmd cmd) where
    showsPrec _ (EvalIO _ ) = showString "EvalIO"
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec _ (MkObj _ _ _) = showString "MkObj"
    showsPrec _ (GetModel _ _) = showString "GetModel"
    showsPrec _ (GetElementalRef _ _ _) = showString "GetElementalRef"
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (DoRerender _) = showString "DoRreender_"
    showsPrec _ (Mutate _ _ _ _) = showString "Mutate"
    showsPrec _ (NotifyMutated _ _) = showString "NotifyMutated"
    showsPrec _ (ResetMutation _) = showString "ResetMutation"
    showsPrec _ (RegisterDOMListener _ _ _ _ _) = showString "RegisterDOMListener"
    showsPrec _ (RegisterReactListener _ _ _ _ _) = showString "RegisterReactListener"
    showsPrec _ (RegisterMountedListener _ _) = showString "RegisterMountedListener"
    showsPrec _ (RegisterRenderedListener _ _) = showString "RegisterRenderedListener"
    showsPrec _ (RegisterNextRenderedListener _ _) = showString "RegisterNextRenderedListener"
    showsPrec _ (RegisterMutatedListener _ _) = showString "RegisterMutatedListener"

------------------------------------------------------
-- | Make a unique named id
mkReactId :: (AsReactor cmd, MonadCommand cmd m)
    => J.JSString -> m ReactId
mkReactId n = delegate $ \fire -> do
    f <- codify fire
    exec' $ MkReactId n f

setRender :: (AsReactor cmd, MonadCommand cmd m)
    => WeakObj s -> Window s () -> m ()
setRender obj win = exec' $ SetRender obj win

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkObj :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> m (Either a (Obj s))
mkObj wid s = delegate $ \fire -> do
    f <- codify fire
    let wid' = wid >>= (instruct . f . Left)
    exec' $ MkObj wid' s (f . Right)

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkObj' :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s () -> s -> m (Obj s)
mkObj' gad s = delegate $ \fire -> do
    f <- codify fire
    exec' $ MkObj gad s f

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
withMkObj :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> (Obj s -> m ()) -> m a
withMkObj wid s k = delegate $ \fire -> do
    f <- codify fire
    k' <- codify k
    let wid' = wid >>= (instruct . f)
    exec' $ MkObj wid' s k'

-- | Rerender the ShimComponent using the current @Entity@ context
rerender :: (MonadReactor p s cmd m) => m ()
rerender = do
    obj <- view _weakObj
    exec' $ Rerender obj

-- | Get the 'Model' and exec actions, using the current @Entity@ context
getModel :: (MonadReactor p s cmd m) => m s
getModel = delegate $ \fire -> do
    Entity obj slf <- ask
    let fire' s = case preview slf s of
            Nothing -> pure ()
            Just s' -> fire s'
    c <- codify fire'
    exec' $ GetModel obj c

-- | Get the event target
-- If a "ref" callback to update 'elementalRef' has not been added;
-- then add it, rerender, then return the EventTarget.
getElementalRef :: (MonadReactor p s cmd m) => ReactId -> m EventTarget
getElementalRef k = delegate $ \fire -> do
    obj <- view _weakObj
    c <- codify fire
    exec' $ GetElementalRef obj k c

-- | Run an arbitrary IO. This should only be used for testing
evalIO :: (MonadReactor p s cmd m) => IO cmd -> m ()
evalIO m = exec' $ EvalIO m

evalIO_ :: (MonadReactor p s cmd m) => IO () -> m ()
evalIO_ m = exec' $ EvalIO (command_ <$> m)

evalIOThen :: (MonadReactor p s cmd m) => IO (m a) -> m a
evalIOThen m = do
    delegate $ \fire -> do
        -- f :: n a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> cmd
        f' <- codify f
        exec' $ EvalIO (f' <$> m)

-- | Update the 'Model' using the current @Entity@ context
mutate :: (MonadReactor p s cmd m) => ReactId -> ModelState s () -> m ()
mutate k m = do
    Entity obj slf <- ask
    cap <- get
    let m' = zoom slf m
    exec' $ Mutate cap k obj (command_ <$> m')

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
mutateThen :: (Also m a, MonadReactor p s cmd m) => ReactId -> ModelState s (m a) -> m a
mutateThen k m = do
    Entity obj slf <- ask
    delegate $ \fire -> do
        let m' = getAls <$> zoom slf (Als <$> m)
            -- f :: m a -> m ()
            f n = n >>= fire
        -- f' :: m a -> cmd
        f' <- codify f
        cap <- get
        exec' $ Mutate cap k obj (f' <$> m')

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
domTrigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => JE.JSRep
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
domTrigger j n goStrict = delegate $ \goLazy -> do
    obj <- view _weakObj
    goLazy' <- codify goLazy
    exec' $ RegisterDOMListener obj j n goStrict goLazy'

-- | A variation of trigger which ignores the event but fires the given arg instead.
domTrigger_ ::
    ( MonadReactor p s cmd m
    )
    => JE.JSRep
    -> J.JSString
    -> a
    -> m a
domTrigger_ j n a = do
    domTrigger j n (const $ pure ())
    pure a

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
doTrigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
doTrigger k n goStrict = delegate $ \goLazy -> do
    obj <- view _weakObj
    goLazy' <- codify goLazy
    exec' $ RegisterReactListener obj k n goStrict goLazy'

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
trigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger k n goStrict = doTrigger k n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice g j = MaybeT (pure $ JE.fromJSR j) >>= g

-- | A variation of trigger which ignores the event but fires the given arg instead.
trigger_ ::
    ( MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> a
    -> m a
trigger_ k n a = do
    doTrigger k n (const $ pure ())
    pure a

-- | Register actions to execute after a render.
-- It is safe to 'postCmd'' a 'Mutate' or 'Rerender'. These command will not
-- trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidMount'
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onMounted ::
    MonadReactor p s cmd m
    => m a
    -> m a
onMounted m = do
    obj <- view _weakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterMountedListener obj c

-- | Register actions to execute after a render.
-- It is safe to 'postCmd'' a 'Mutate' or 'Rerender'. These command will not
-- trigger another rendered event.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onRendered ::
    MonadReactor p s cmd m
    => m a
    -> m a
onRendered m = do
    obj <- view _weakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterRenderedListener obj c

onNextRendered ::
    MonadReactor p s cmd m
    => m a -> m a
onNextRendered m = do
    obj <- view _weakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterNextRenderedListener obj c

-- | FIXME: Add reactId here to onMutated can decide what to do based
-- on which component was mutated?

-- | Register actions to execute after the state has been updated with TickState.
-- To prevent infinite loops, if this 'onMutated' causes in another 'mutate'
-- another 'onMutated' callback will NOT be fired.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onMutated ::
    MonadReactor p s cmd m
    => (ReactId -> m a) -> m a
onMutated f = do
    obj <- view _weakObj
    delegate $ \fire -> do
        g <- codify ((fire =<<) . f)
        exec' $ RegisterMutatedListener obj g

onMutated' ::
    MonadReactor p s cmd m
    => m a -> m a
onMutated' f = onMutated (const f)
