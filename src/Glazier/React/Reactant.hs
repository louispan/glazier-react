{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Glazier.React.Reactant where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad.Trans.Maybe
import Data.IORef.Extras
import qualified Data.JSString as J
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.Logger
import Glazier.React.Common
import Glazier.React.Obj.Internal
import Glazier.React.Plan.Internal
import Glazier.React.ReactId
import Glazier.React.Reactor
import System.Mem.Weak

-----------------------------------------------------------------

type CmdReactant c =
    ( Cmd' [] c -- required by 'command_'
    , Cmd' IO c -- required by 'MonadCodify' for @ProgramT IO@
    , Cmd' Reactant c
    , Cmd (LogLine J.JSString) c
    )

-- | Describes the effects required by 'Widget' to manipulate 'Obj'.
-- 'Reactant' is not a functor because of the @Widget c@ in 'MkObj'
-- which is in a positive agument position.
data Reactant c where

    MkReactId :: (ReactId -> c) -> Reactant c

    -- Turn some handling function into a 'Handler'.
    -- The reason for the two handling functions is detailed in 'Glazier.React.Reactant.Exec.execMkHandler'
    -- Glazier will try to return the same 'Handler for the same input functions as much as possible
    -- so that it will be relatively efficient to use this function on every rerender.
    MkHandler :: NFData a
        => Weak (IORef Plan)
        -> (J.JSVal -> MaybeT IO a)
        -> (a -> c)
        -> (Handler -> c)
        -> Reactant c

    -- | Turn 'Handler' into a 'J.Callback' so it can be called from JS.
    -- Glazier will try to return the same 'J.Callback' for the same input functions as much as possible.
    -- so that it will be relatively efficient to use this function on every rerender.
    MkListener ::
        Weak (IORef Plan)
        -> Handler
        -> (Listener -> c)
        -> Reactant c

    -- | Make a fully initialized 'Obj' from a widget and model
    -- 'Reactant' is not a functor because of the @Widget@ in 'MkObj'
    -- which is in a positive agument position.
    MkObj :: Widget s c () -> LogName -> (IORef Notifier, Weak (IORef Notifier), MVar s, Weak (MVar s)) -> (Obj s -> c) -> Reactant c

    -- Notifies any watchers (from 'readWeakObj')
    -- that the model has changed so that the watchers can rerender.
    -- Any rerendering is batched and might be be done immediately
    NotifyDirty :: Weak (IORef Notifier) -> Reactant c

-- instance (IsString str, Semigroup str) => ShowIO str (Reactant c) where
--     showsPrecIO p (MkHandler this _ _ _) = showParenIO (p >= 11) $ (showStr "MkHandler " .) <$> (showsIO this)
--     showsPrecIO p (MkListener this _ _) = showParenIO (p >= 11) $ (showStr "MkListener " .) <$> (showsIO this)
--     showsPrecIO p (MkObj _ logname _ _) = showParenIO (p >= 11) $ (showStr "MkObj " .) <$> (showsIO logname)
--     showsPrecIO p (ReadObj this _ _) = showParenIO (p >= 11) $ (showStr "ReadObj " .) <$> (showsIO this)
--     showsPrecIO p (Mutate this _ req _) = showParenIO (p >= 11) $ (\x -> (showStr "Mutate ") . x . (showFromStr " ") . (showsStr req)) <$> (showsIO this)
