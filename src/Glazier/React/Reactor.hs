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
    ( ReactorCmds
    , AsReactor
    , ReactorCmd(..)
    , trigger
    , trigger'
    , onRendered
    , hdlElementalRef
    , rerender
    , getScene
    , tickScene
    , mkSubject
    ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import Data.Maybe
import Data.Tagged
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Entity
import Glazier.React.MkId
import Glazier.React.Notice
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Widget
import qualified JavaScript.Extras as JE

-- -- | Convenience function to avoid @TypeApplications@ when using OverloadedLists
-- cmds :: AsFacet [c] c => [c] -> c
-- cmds = cmd @[]

-----------------------------------------------------------------

type ReactorCmds cmd =
    '[ ReactorCmd cmd
    ]

type AsReactor cmd =
    ( AsFacet [cmd] cmd
    , AsFacet (ReactorCmd cmd) cmd
    )

data ReactorCmd cmd where
    -- | Rerender a ShimComponent using the given state.
    Rerender :: Subject s -> ReactorCmd cmd
    -- | Make an initialized 'Subject' for a given model using the given
    -- 'Window' rendering function.
    -- The original window should be dropped and the 'Widget' reduced to just a
    -- 'Gadget' to emphasis the fact that the 'Window' was used up.
    MkSubject :: Widget cmd s s () -> s -> (Subject s -> cmd) -> ReactorCmd cmd
    MkHandler :: cmd -> (IO () -> cmd) -> ReactorCmd cmd
    -- | Convert a callback to a @JE.JSRep -> IO ()@
    MkHandler1 :: NFData a
        => (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ((JE.JSRep -> IO ()) -> cmd)
        -> ReactorCmd cmd
    -- Generate a list of commands from reading a scene.
    GetScene :: Subject s -> (Scene s -> cmd) -> ReactorCmd cmd
    -- Update and rerender a scene.
    TickScene :: Subject s -> State (Scene s) () -> ReactorCmd cmd

instance Show cmd => Show (ReactorCmd cmd) where
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (TickScene _ _) = showString "TickScene"
    showsPrec _ (GetScene _ _) = showString "GetScene"
    showsPrec p (MkHandler c _) = showParen (p >= 11) $
        showString "MkHandler " . shows c
    showsPrec _ (MkHandler1 _ _ _) = showString "MkHandler1"
    showsPrec _ (MkSubject _ _ _) = showString "MkSubject"

------------------------------------------------------

-- | Create a callback and add it to this elemental's dlist of listeners.
trigger_ ::
    ( NFData a
    , AsReactor cmd
    , MonadReader (Entity p s) m
    , MonadCommand cmd m
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> ElementalId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> (a -> m ())
    -> m ()
trigger_ l eid n goStrict goLazy = do
    Entity sbj _ <- ask
    goLazy' <- codify goLazy
    postcmd' $ MkHandler1 goStrict goLazy' $ \act ->
        -- save the created action handler in the elemental
        let updateElemental = _plan._elementals.at eid %= (Just . addListener . fromMaybe newElemental)
            addListener = _listeners.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
            addAction acts = acts & l %~ (*> act)
        in command' $ TickScene sbj updateElemental

-- | Create a callback for a 'JE.JSRep' and add it to this elementals's dlist of listeners.
-- You probably want to use 'trigger'' since most React callbacks return a 'Notice',
-- except for the "ref" callback, in which case you probably want to use 'trackElemental'.
trigger ::
    ( NFData a
    , AsReactor cmd
    , MonadReader (Entity p s) m
    , MonadCommand cmd m
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> ElementalId
    -> J.JSString
    -> (JE.JSRep -> MaybeT IO a)
    -> m a
trigger l eid n goStrict = delegate $ trigger_ l eid n goStrict

-- | Create a callback for a 'Notice' and add it to this elementals's dlist of listeners.
trigger' ::
    ( NFData a
    , AsReactor cmd
    , MonadReader (Entity p s) m
    , MonadCommand cmd m
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> ElementalId
    -> J.JSString
    -> (Notice -> MaybeT IO a)
    -> m a
trigger' l eid n goStrict = trigger l eid n $ handlesNotice goStrict
  where
    handlesNotice :: (Notice -> MaybeT IO a) -> (JE.JSRep -> MaybeT IO a)
    handlesNotice k j = MaybeT (pure $ JE.fromJSR j) >>= k

-- | Register actions to execute after a render.
-- Do not 'postcmd'' 'TickScene' or 'Rerender' otherwise it will go into infinite render loops.
--
-- NB. This is trigged by react 'componentDidUpdate' and 'componentDidMount'
-- so it is also called for the initial render.
-- See jsbits/react.js hgr$shimComponent.
-- These callbacks are called after the ref callback by React
-- See https://reactjs.org/docs/refs-and-the-dom.html.
onRendered ::
    ( AsReactor cmd
    , MonadReader (Entity p s) m
    , MonadCommand cmd m
    )
    => Lens' (Tagged "Once" (IO ()), Tagged "Always" (IO ())) (IO ())
    -> m ()
    -> m ()
onRendered l m = do
    sbj <- view _subject
    c <- codify' m
    postcmd' $ MkHandler c $ \act ->
            -- save the rendered action handler in the plan
            let addListener = _plan._doOnRendered.l %= (*> act)
            in command' $ TickScene sbj addListener

-- | This adds a ReactJS "ref" callback assign the ref into an 'EventTarget'
-- for the elemental in the plan, so that the elemental '_targetRef' can be used.
hdlElementalRef ::
    (AsReactor cmd
    , MonadReader (Entity p s) m
    , MonadCommand cmd m
    ) => ElementalId
    -> m ()
hdlElementalRef eid = trigger _always eid "ref" (pure . JE.fromJSR) >>= hdlRef
  where
    hdlRef x = do
        sbj <- view _subject
        postcmd' . TickScene sbj $ _plan._elementals.ix eid._elementalRef .= x

-- | Rerender the ShimComponent using the current @Entity@ context
rerender :: (AsReactor cmd, MonadReader (Entity p s) m, MonadCommand cmd m) => m ()
rerender = do
    sbj <- view _subject
    postcmd' $ Rerender sbj

-- | Get the 'Scene' and exec actions, using the current @Entity@ context
getScene ::
    (AsReactor cmd, MonadReader (Entity p s) m, MonadCommand cmd m)
    => (Scene s -> m ()) -> m ()
getScene go = do
    Entity sbj slf <- ask
    let go' s = case preview (editSceneModel slf) s of
            Nothing -> pure ()
            Just s' -> go s'
    c <- codify go'
    postcmd' $ GetScene sbj c

-- | Update the 'Scene' using the current @Entity@ context
tickScene ::
    (AsReactor cmd, MonadReader (Entity p s) m, MonadCommand cmd m)
    => State (Scene s) () -> m ()
tickScene m = do
    Entity sbj slf <- ask
    let m' = zoom (editSceneModel slf) m
    postcmd' $ TickScene sbj m'

mkSubject ::
    (AsReactor cmd, MonadCommand cmd m)
    => Widget cmd s s () -> s -> (Subject s -> m ()) -> m ()
mkSubject wid s k = do
    k' <- codify k
    postcmd' $ MkSubject wid s k'
