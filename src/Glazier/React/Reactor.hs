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
import Control.Monad.Trans.AState.Strict
import Data.Diverse.Lens
import Glazier.React.Scene
import Glazier.React.Widget
import qualified JavaScript.Extras as JE

-- -- | Convenience function to avoid @TypeApplications@ when using OverloadedLists
-- cmds :: AsFacet [c] c => [c] -> c
-- cmds = cmd @[]

-----------------------------------------------------------------

type ReactorCmds c =
    '[ [c]
    , ReactorCmd c
    , CD.Disposable
    ]

type AsReactor c =
    ( AsFacet [c] c
    , AsFacet (ReactorCmd c) c
    , AsFacet CD.Disposable c
    )

data ReactorCmd c where
    -- | Rerender a ShimComponent using the given state.
    Rerender :: Subject s -> ReactorCmd c
    -- The executor of this command will automatically check 'rerender' at the end of this state tick
    TickState :: Subject s -> (AState (Scenario c s) ()) -> ReactorCmd c
    MkAction :: c -> (IO () -> c) -> ReactorCmd c
    -- | Convert a callback to a @JE.JSRep -> IO ()@
    MkAction1 :: NFData a
        => (JE.JSRep -> IO (Maybe a))
        -> (a -> c)
        -> ((JE.JSRep -> IO ()) -> c)
        -> ReactorCmd c
    -- | Make an initialized 'Subject' for a given model using the given
    -- 'Window' rendering function.
    -- The original window should be dropped and the 'Widget' reduced to just a
    -- 'Gadget' to emphasis the fact that the 'Window' was used up.
    MkSubject :: Widget c s s () -> s -> (Subject s -> c) -> ReactorCmd c

instance Show c => Show (ReactorCmd c) where
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (TickState _ _) = showString "TickState"
    showsPrec p (MkAction c _) = showParen (p >= 11) $
        showString "MkAction " . shows c
    showsPrec _ (MkAction1 _ _ _) = showString "MkAction1"
    showsPrec _ (MkSubject _ _ _) = showString "MkSubject"

