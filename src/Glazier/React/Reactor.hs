{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeOperators #-}

module Glazier.React.Reactor where

import Control.DeepSeq
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified GHCJS.Types as J
import Glazier.React.EventTarget
import Glazier.React.Obj
import Glazier.React.ReactId
import Glazier.React.ReadIORef
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE
-----------------------------------------------------------------

type AsReactor cmd =
    ( AsFacet [cmd] cmd -- implicity required by 'MonadCodify'
    , AsFacet (ReactorCmd cmd) cmd
    )

type ModelState s = StateT s ReadIORef

-- | NB. 'ReactorCmd' is not a functor because of the @Widget cmd@ in 'MkObj'
data ReactorCmd cmd where
    -- run arbitrary IO, should only be used for debugging
    -- FIXME: Rename to RunIO
#ifdef DEBUG_REACT
    DebugIO :: IO cmd -> ReactorCmd cmd
#endif
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
    -- | Private: Calls the model mutatedListener
    -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    NotifyMutated :: ReactId -> WeakObj s -> ReactorCmd cmd
    -- | Private: Resets the mutated state for this ReactId
    -- If there are no more pending mutations, then rerender.
    -- Should only have one of this per ReactId for multiple Mutate with the same ReactId
    ResetMutation :: ReactId -> WeakObj s -> ReactorCmd cmd
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
#ifdef DEBUG_REACT
    showsPrec _ (DebugIO _ ) = showString "DebugIO"
#endif
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
    showsPrec _ (ResetMutation _ _) = showString "ResetMutation"
    showsPrec _ (RegisterDOMListener _ _ _ _ _) = showString "RegisterDOMListener"
    showsPrec _ (RegisterReactListener _ _ _ _ _) = showString "RegisterReactListener"
    showsPrec _ (RegisterMountedListener _ _) = showString "RegisterMountedListener"
    showsPrec _ (RegisterRenderedListener _ _) = showString "RegisterRenderedListener"
    showsPrec _ (RegisterNextRenderedListener _ _) = showString "RegisterNextRenderedListener"
    showsPrec _ (RegisterMutatedListener _ _) = showString "RegisterMutatedListener"
