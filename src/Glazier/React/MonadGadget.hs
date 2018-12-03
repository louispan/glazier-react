{-# LANGUAGE CPP #-}
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

module Glazier.React.MonadGadget
    ( MonadGadget
    , ModelState
    , debugIO
    , debugIO_
    , debugIOThen
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
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.EventTarget
import Glazier.React.Notice
import Glazier.React.Obj
import Glazier.React.ReactId
import Glazier.React.Reactor
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE


type MonadGadget cmd o s m =
    ( AsReactor cmd
    , MonadReader (Entity o s) m
    , MonadCommand cmd m
    , MonadState J.JSString m
    )

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
rerender :: (MonadGadget cmd o s m) => m ()
rerender = do
    obj <- view _weakObj
    exec' $ Rerender obj

-- | Get the 'Model' and exec actions, using the current @Entity@ context
getModel :: (MonadGadget cmd o s m) => m s
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
getElementalRef :: (MonadGadget cmd o s m) => ReactId -> m EventTarget
getElementalRef k = delegate $ \fire -> do
    obj <- view _weakObj
    c <- codify fire
    exec' $ GetElementalRef obj k c

-- | Run an arbitrary IO. This should only be used for testing
#ifdef DEBUG_REACT

debugIO :: (MonadGadget cmd o s m) => IO cmd -> m ()
debugIO m = exec' $ DebugIO m

debugIO_ :: (MonadGadget cmd o s m) => IO () -> m ()
debugIO_ m = exec' $ DebugIO (command_ <$> m)

debugIOThen :: (MonadGadget cmd o s m) => IO (m a) -> m a
debugIOThen m = do
    delegate $ \fire -> do
        -- f :: n a -> m ()
        let f n = n >>= fire
        -- f' :: m a -> cmd
        f' <- codify f
        exec' $ DebugIO (f' <$> m)

#else

debugIO :: (MonadGadget cmd o s m) => IO cmd -> m ()
debugIO _ = pure ()

debugIO_ :: (MonadGadget cmd o s m) => IO () -> m ()
debugIO_ _ = pure ()

debugIOThen :: (MonadGadget cmd o s m) => IO (m a) -> m a
debugIOThen _ = finish (pure ())

#endif

{-# WARNING debugIO, debugIO_, debugIOThen "Use this for debugging only. It will be disabled when DEBUG_REACT is not set" #-}

-- | Update the 'Model' using the current @Entity@ context
mutate :: (MonadGadget cmd o s m) => ReactId -> ModelState s () -> m ()
mutate k m = do
    Entity obj slf <- ask
    cap <- get
    let m' = zoom slf m
    exec' $ Mutate cap k obj (command_ <$> m')

-- | Update the 'Model' using the current @Entity@ context,
-- and also return the next action to execute.
mutateThen :: (Also m a, MonadGadget cmd o s m) => ReactId -> ModelState s (m a) -> m a
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
-- 'domTrigger' does not expect a 'Notice' as it is not part of React.
-- Contrast with 'trigger' which expects a 'Notice'.
domTrigger ::
    ( NFData a
    , MonadGadget cmd o s m
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
    ( MonadGadget cmd o s m
    )
    => JE.JSRep
    -> J.JSString
    -> a
    -> m a
domTrigger_ j n a = do
    domTrigger j n (const $ pure ())
    pure a

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- Not exposed because all react listeners result in 'Notice'.
__trigger ::
    ( NFData a
    , MonadGadget cmd o s m
    )
    => ReactId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
__trigger k n goStrict = delegate $ \goLazy -> do
    obj <- view _weakObj
    goLazy' <- codify goLazy
    exec' $ RegisterReactListener obj k n goStrict goLazy'

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
-- Contrast with 'domTrigger' which expects a 'JE.JSRep'.
trigger ::
    ( NFData a
    , MonadGadget cmd o s m
    )
    => ReactId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger k n goStrict = __trigger k n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice g j = MaybeT (pure $ JE.fromJSR j) >>= g

-- | A variation of 'trigger' which ignores the event but fires the given arg instead.
trigger_ ::
    ( MonadGadget cmd o s m
    )
    => ReactId
    -> J.JSString
    -> a
    -> m a
trigger_ k n a = do
    -- using '__trigger' to bypass conversion to 'Notice' in 'trigger'
    __trigger k n (const $ pure ())
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
    MonadGadget cmd o s m
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
    MonadGadget cmd o s m
    => m a
    -> m a
onRendered m = do
    obj <- view _weakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterRenderedListener obj c

-- | Same as 'onRendered' but the action only occurs once on the next rerender.
onNextRendered ::
    MonadGadget cmd o s m
    => m a -> m a
onNextRendered m = do
    obj <- view _weakObj
    delegate $ \fire -> do
        c <- codify' (m >>= fire)
        exec' $ RegisterNextRenderedListener obj c

-- | Register actions to execute after the state has been updated with 'mutate'.
-- The callback is called with the 'ReactId' that was passed into 'mutate'.
-- To prevent infinite loops, if this 'onMutated' causes another 'mutate'
-- with the same 'ReactId', then another 'onMutated' callback will NOT be fired.
onMutated ::
    MonadGadget cmd o s m
    => (ReactId -> m a) -> m a
onMutated f = do
    obj <- view _weakObj
    delegate $ \fire -> do
        g <- codify ((fire =<<) . f)
        exec' $ RegisterMutatedListener obj g

-- | Variation of 'onMutated' where you don't care about the 'ReactId' that caused the rerender.
onMutated' ::
    MonadGadget cmd o s m
    => m a -> m a
onMutated' f = onMutated (const f)
