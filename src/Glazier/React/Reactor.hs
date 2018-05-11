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
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.Command
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Widget
import qualified JavaScript.Extras as JE

-- -- | Convenience function to avoid @TypeApplications@ when using OverloadedLists
-- cmds :: AsFacet [c] c => [c] -> c
-- cmds = cmd @[]

-----------------------------------------------------------------

type ReactorCmds cmd =
    '[ [cmd]
    , ReactorCmd cmd
    , CD.Disposable
    ]

type AsReactor cmd =
    ( AsFacet [cmd] cmd
    , AsFacet (ReactorCmd cmd) cmd
    , AsFacet CD.Disposable cmd
    )

type MonadScenario s cmd m =
    ( MonadState s m
    , HasCommands' s cmd
    , MonadReader (Scene s) m
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
    Scenario :: Subject s -> ReaderT (Scene s) (State (DL.DList cmd)) () -> ReactorCmd cmd
    -- Update and rerneder a scene.
    TickScene :: Subject s -> State (Scene s) () -> ReactorCmd cmd

instance Show cmd => Show (ReactorCmd cmd) where
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (TickScene _ _) = showString "TickScene"
    showsPrec _ (Scenario _ _) = showString "Scenario"
    showsPrec p (MkHandler c _) = showParen (p >= 11) $
        showString "MkHandler " . shows c
    showsPrec _ (MkHandler1 _ _ _) = showString "MkHandler1"
    showsPrec _ (MkSubject _ _ _) = showString "MkSubject"

