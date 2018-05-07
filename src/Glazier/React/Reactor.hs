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
import Control.Monad.State.Strict
import Data.Diverse.Lens
import Glazier.React.Scene
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

data ReactorCmd cmd where
    -- | Rerender a ShimComponent using the given state.
    Rerender :: Subject s -> ReactorCmd cmd
    -- | Make an initialized 'Subject' for a given model using the given
    -- 'Window' rendering function.
    -- The original window should be dropped and the 'Widget' reduced to just a
    -- 'Gadget' to emphasis the fact that the 'Window' was used up.
    MkSubject :: Widget cmd s s () -> s -> (Subject s -> cmd) -> ReactorCmd cmd
    MkAction :: cmd -> (IO () -> cmd) -> ReactorCmd cmd
    -- | Convert a callback to a @JE.JSRep -> IO ()@
    MkAction1 :: NFData a
        => (JE.JSRep -> IO (Maybe a))
        -> (a -> cmd)
        -> ((JE.JSRep -> IO ()) -> cmd)
        -> ReactorCmd cmd
    -- ReadState :: Subject s -> ReaderT (Scene s) (State (DL.DList cmd)) () -> ReactorCmd cmd
    TickState :: Subject s -> State (Scenario cmd s) () -> ReactorCmd cmd

instance Show cmd => Show (ReactorCmd cmd) where
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (TickState _ _) = showString "TickState"
    -- showsPrec _ (ReadState _ _) = showString "ReadState"
    showsPrec p (MkAction c _) = showParen (p >= 11) $
        showString "MkAction " . shows c
    showsPrec _ (MkAction1 _ _ _) = showString "MkAction1"
    showsPrec _ (MkSubject _ _ _) = showString "MkSubject"

