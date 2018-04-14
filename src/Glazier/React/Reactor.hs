{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Reactor where

import Control.DeepSeq
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.AState.Strict
import Data.Diverse.Lens
import Glazier.React.Scene
import Glazier.React.Window
import qualified JavaScript.Extras as JE

-- | convert a request type to a command type.
-- This is used for commands that doesn't have a continuation.
-- Ie. commands that doesn't "returns" a value from running an effect.
-- Use 'cmd'' for commands that require a continuation ("returns" a value).
cmd :: (AsFacet c' c) => c' -> c
cmd = review facet

-- | A variation of 'cmd' for commands with a type variable @c@,
-- which is usually commands that are containers of command,
-- or commands that require a continuation
-- Eg. commands that "returns" a value from running an effect.
-- 'cmd'' is usually used with with the 'Cont' monad to help
-- create the continuation.
--
-- @
-- post $ (`runCont` id) $ do
--     a <- cont $ cmd' . GetSomething
--     pure . cmd $ DoSomething (f a)
-- @
cmd' :: (AsFacet (c' c) c) => c' c -> c
cmd' = cmd

-- -- | Convenience function to avoid @TypeApplications@ when using OverloadedLists
-- cmds :: AsFacet [c] c => [c] -> c
-- cmds = cmd' @[]

-----------------------------------------------------------------

type ReactorCmds c =
    '[ [c]
    , Rerender
    , TickState c
    , MkAction1 c
    , MkAction c
    , MkShimCallbacks
    , CD.Disposable
    ]

type AsReactor c =
    ( AsFacet [c] c
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    , AsFacet (MkAction c) c
    , AsFacet MkShimCallbacks c
    , AsFacet CD.Disposable c
    )

-- | Rerender a ShimComponent using the given state.
data Rerender where
    Rerender ::
        Subject s
        -> Rerender

instance Show Rerender where
    showsPrec _ _ = showString "Rerender"

-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: (MonadState (Scenario c s) m) => m ()
dirty = _scene._plan._currentFrameNum %= JE.safeModularIncrement

-- The executor of this command will automatically check 'rerender' at the end of this state tick
data TickState c where
    TickState ::
        Subject s
        -> (AState (Scenario c s) ())
        -> TickState c

instance Show (TickState c) where
    showsPrec _ _ = showString "TickState"

-- | Convert a command to an IO action
data MkAction c = MkAction c (IO () -> c)

instance Show c => Show (MkAction c) where
    showsPrec d (MkAction c _) = showParen (d >= 11) $
        showString "MkAction " . shows c

-- | Convert a callback to a @JE.JSRep -> IO ()@
data MkAction1 c where
    MkAction1 :: NFData a
        => (JE.JSRep -> IO (Maybe a))
        -> (a -> c)
        -> ((JE.JSRep -> IO ()) -> c)
        -> MkAction1 c

instance Show (MkAction1 c) where
    showsPrec _ _ = showString "MkAction1"

-- | Make the 'ShimCallbacks' for this 'Plan' using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used up.
data MkShimCallbacks where
    MkShimCallbacks ::
        Subject s
        -> (Window s ())
        -> MkShimCallbacks

instance Show MkShimCallbacks where
    showsPrec _ _ = showString "MkShimCallbacks"
