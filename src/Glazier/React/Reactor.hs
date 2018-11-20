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
    , mkObj2
    , mkObj2'
    , withMkObj2
    , mkWeakObj
    , keepAliveObjUntilNextRender
    , getModel
    , getElementalRef
    , rerender
    , tickModel
    , tickModelThen
    , domTrigger
    , domTrigger_
    , trigger
    , trigger_
    , onMounted
    , onRendered
    , onNextRendered
    , onTicked
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
    )

type ModelState s = StateT s ReadIORef

-- | NB. 'ReactorCmd' is not a functor because of the @Widget cmd@ in 'MkObj'
data ReactorCmd cmd where
    -- run arbitrary IO, should only be used for debugging
    EvalIO :: IO cmd -> ReactorCmd cmd
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> cmd) -> ReactorCmd cmd
    -- | the the rendering function in a Obj, replace any existing render callback
    SetRender :: WeakObj s -> Window s () -> ReactorCmd cmd
    -- | Make a fully initialized subject (with ShimCallbacks) from a widget spec and state
    MkObj :: Widget cmd s s () -> s -> (Obj s -> cmd) -> ReactorCmd cmd
    MkObj2 :: Widget cmd s s () -> s -> (Obj s -> cmd) -> ReactorCmd cmd
    -- make a weak subject from a subject
    MkWeakObj :: Obj s -> (WeakObj s -> cmd) -> ReactorCmd cmd
    -- | Keep subject alive until the next rerender
    KeepAliveObjUntilNextRender :: WeakObj s -> Obj a -> ReactorCmd cmd
    -- | Generate a list of commands from reading the model.
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
    -- | Update and rerender.
    TickModel :: WeakObj s -> ModelState s cmd -> ReactorCmd cmd
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
    RegisterTickedListener ::
        WeakObj s
        -> cmd
        -> ReactorCmd cmd

instance Show (ReactorCmd cmd) where
    showsPrec _ (EvalIO _ ) = showString "EvalIO"
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec _ (MkObj _ _ _) = showString "MkObj"
    showsPrec _ (MkObj2 _ _ _) = showString "MkObj2"
    showsPrec _ (MkWeakObj _ _) = showString "MkWeakObj"
    showsPrec _ (KeepAliveObjUntilNextRender _ _) = showString "KeepAliveObjUntilNextRender"
    showsPrec _ (GetModel _ _) = showString "GetModel"
    showsPrec _ (GetElementalRef _ _ _) = showString "GetElementalRef"
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (TickModel _ _) = showString "TickModel"
    showsPrec _ (RegisterDOMListener _ _ _ _ _) = showString "RegisterDOMListener"
    showsPrec _ (RegisterReactListener _ _ _ _ _) = showString "RegisterReactListener"
    showsPrec _ (RegisterMountedListener _ _) = showString "RegisterMountedListener"
    showsPrec _ (RegisterRenderedListener _ _) = showString "RegisterRenderedListener"
    showsPrec _ (RegisterNextRenderedListener _ _) = showString "RegisterNextRenderedListener"
    showsPrec _ (RegisterTickedListener _ _) = showString "RegisterTickedListener"

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

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkObj2 :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> m (Either a (Obj s))
mkObj2 wid s = delegate $ \fire -> do
    f <- codify fire
    let wid' = wid >>= (instruct . f . Left)
    exec' $ MkObj wid' s (f . Right)

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkObj2' :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s () -> s -> m (Obj s)
mkObj2' gad s = delegate $ \fire -> do
    f <- codify fire
    exec' $ MkObj gad s f

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
withMkObj2 :: (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s a -> s -> (Obj s -> m ()) -> m a
withMkObj2 wid s k = delegate $ \fire -> do
    f <- codify fire
    k' <- codify k
    let wid' = wid >>= (instruct . f)
    exec' $ MkObj wid' s k'

-- -- | Add a constructed subject to a parent widget
-- addObj :: (MonadReactor p ss cmd m)
--     => Widget cmd s s a
--     -> s
--     -> (Obj s -> StateT (Model ss) ReadIORef ())
--     -> m a
-- addObj wid s f = mkObj wid s $ \obj -> tickModel $ f obj

-- | Make an initialized 'Obj' for a given model using the given 'Widget'.
mkWeakObj :: (AsReactor cmd, MonadCommand cmd m)
    => Obj s -> m (WeakObj s)
mkWeakObj s = delegate $ \fire -> do
    f <- codify fire
    exec' $ MkWeakObj s f

-- | Schedule cleanup of the callbacks when the parent widget is rerendered.
keepAliveObjUntilNextRender ::
    (MonadReactor p s cmd m)
    => Obj a -> m ()
keepAliveObjUntilNextRender s = do
    obj <- view _weakObj
    exec' $ KeepAliveObjUntilNextRender obj s

-- | Rerender the ShimComponent using the current @Entity@ context
rerender :: (MonadReactor p s cmd m) => m ()
rerender = do
    obj <- view _weakObj
    exec' $ Rerender obj

-- | Get the 'Model' and exec actions, using the current @Entity@ context
getModel :: (MonadReactor p s cmd m) => m s
getModel = delegate $ \k -> do
    Entity obj slf <- ask
    let k' s = case preview slf s of
            Nothing -> pure ()
            Just s' -> k s'
    c <- codify k'
    exec' $ GetModel obj c

-- | Get the event target
-- If a "ref" callback to update 'elementalRef' has not been added;
-- then add it, rerender, then return the EventTarget.
getElementalRef :: (MonadReactor p s cmd m) => ReactId -> m EventTarget
getElementalRef ri = delegate $ \k -> do
    obj <- view _weakObj
    c <- codify k
    exec' $ GetElementalRef obj ri c

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
tickModel :: (MonadReactor p s cmd m) => ModelState s () -> m ()
tickModel m = do
    Entity obj slf <- ask
    let m' = zoom slf m
    exec' $ TickModel obj (command_ <$> m')

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
tickModelThen :: (Also m a, MonadReactor p s cmd m) => ModelState s (m a) -> m a
tickModelThen m = do
    Entity obj slf <- ask
    delegate $ \fire -> do
        let m' = getAls <$> zoom slf (Als <$> m)
            -- f :: m a -> m ()
            f n = n >>= fire
        -- f' :: m a -> cmd
        f' <- codify f
        exec' $ TickModel obj (f' <$> m')

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
doTrigger ri n goStrict = delegate $ \goLazy -> do
    obj <- view _weakObj
    goLazy' <- codify goLazy
    exec' $ RegisterReactListener obj ri n goStrict goLazy'

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
trigger ::
    ( NFData a
    , MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger ri n goStrict = doTrigger ri n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | A variation of trigger which ignores the event but fires the given arg instead.
trigger_ ::
    ( MonadReactor p s cmd m
    )
    => ReactId
    -> J.JSString
    -> a
    -> m a
trigger_ ri n a = do
    doTrigger ri n (const $ pure ())
    pure a

-- | Register actions to execute after a render.
-- It is safe to 'postCmd'' a 'TickModel' or 'Rerender'. These command will not
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
-- It is safe to 'postCmd'' a 'TickModel' or 'Rerender'. These command will not
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

-- | Register actions to execute after the state has been updated with TickState.
-- It is safe to 'postCmd'' another 'TickModel', another onRendered event will
-- not be generated.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onTicked ::
    MonadReactor p s cmd m
    => m a
    -> m a
onTicked m = do
    obj <- view _weakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterTickedListener obj c
