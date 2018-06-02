{-# LANGUAGE GADTs #-}

module Glazier.React.Reactor.Internal where

import Control.DeepSeq
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified GHCJS.Types as J
import Glazier.React.ReactId
import Glazier.React.ReadIORef
import Glazier.React.Scene
import Glazier.React.Subject
import Glazier.React.Widget
import Glazier.React.Window
import qualified JavaScript.Extras as JE

-----------------------------------------------------------------

type SceneState s = StateT (Scene s) ReadIORef

-- | NB. ReactorCmd is not a functor.
data ReactorCmd cmd where
    -- | Make a unique named id
    MkReactId :: J.JSString -> (ReactId -> cmd) -> ReactorCmd cmd
    -- | the the rendering function in a Subject
    SetRender :: Subject s -> Window s () -> ReactorCmd cmd
    -- | Make a fully initialized subject (with ShimCallbacks) from a widget spec and state
    MkSubject :: Widget cmd s s () -> s -> (Subject s -> cmd) -> ReactorCmd cmd
    -- | Rerender a ShimComponent using the given state.
    Rerender :: Subject s -> ReactorCmd cmd
    -- | Generate a list of commands from reading a scene.
    GetScene :: Subject s -> (Scene s -> cmd) -> ReactorCmd cmd
    -- | Update and rerender a scene.
    TickScene :: Subject s -> SceneState s cmd -> ReactorCmd cmd
    -- | DoReadIORef :: IORef a -> (a -> cmd) -> ReactorCmd
    MkHandler :: cmd -> (IO () -> cmd) -> ReactorCmd cmd
    -- | Convert a callback to a @JE.JSRep -> IO ()@
    MkHandler1 :: NFData a
        => (JE.JSRep -> MaybeT IO a)
        -> (a -> cmd)
        -> ((JE.JSRep -> IO ()) -> cmd)
        -> ReactorCmd cmd

instance Show cmd => Show (ReactorCmd cmd) where
    showsPrec p (MkReactId s _) = showParen (p >= 11) $
        showString "MkReactId " . shows s
    showsPrec _ (SetRender _ _ ) = showString "SetRender"
    showsPrec _ (MkSubject _ _ _) = showString "MkSubject"
    showsPrec _ (Rerender _) = showString "Rerender"
    showsPrec _ (GetScene _ _) = showString "GetScene"
    showsPrec _ (TickScene _ _) = showString "TickScene"
    showsPrec p (MkHandler c _) = showParen (p >= 11) $
        showString "MkHandler " . shows c
    showsPrec _ (MkHandler1 _ _ _) = showString "MkHandler1"
