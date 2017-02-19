{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import Data.List
import Data.Monoid
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Prim as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Element as R
import qualified Glazier.React.Markup as R
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Prelude as PP
import qualified Todo.App as TD.App
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
main :: IO ()
main = do
    -- Create a 'Pipes.Concurrent' mailbox for receiving actions from html events.
    -- NB. using 'PC.bounded 1' also works without deadlocks, but doesn't save memory
    -- blocked events are kept by the GHCJCS runtime.
    (output, input) <- liftIO . PC.spawn $ PC.unbounded

    -- It is not trivial to call arbitrary Haskell functions from Javascript
    -- A hacky way is to create a Callback and assign it to a global registry.
    inputChangeFirer' <- mkActionCallback output TD.App.inputChangeFirer
    inputSubmitFirer' <- mkActionCallback output TD.App.inputSubmitFirer
    toggleCompleteAllFirer' <- mkActionCallback output TD.App.toggleCompleteAllFirer

    -- TODO: How to make sure the correct handlers are passed into the correct place?
    let initialState = TD.App.Model
            (TD.Input.Model
                 "input"
                 "hello world!"
                 inputChangeFirer'
                 inputSubmitFirer')
            0
            mempty
            toggleCompleteAllFirer'
            Nothing

    -- Make a MVar so render can get the latest state
    currentState <- newMVar initialState

    -- Setup the render callback
    -- render <- syncCallback1' (view G._WindowT' (jsval <$> counterWindow) . R.unsafeCoerceReactElement)
    doRender <- J.syncCallback1' $ \_ -> do
        s <- readMVar currentState
        xs <- view G._WindowT' (R.renderedWindow TD.App.window) s
        liftIO $ void $ atomically $ PC.send output TD.App.ReleaseCallbacksAction -- FIXME: not quite long enough
        J.jsval <$> R.mkCombinedElements xs
    void $ js_globalListen "renderHaskell" doRender

    -- trigger a render now that the render callback is initialized
    js_globalShout "forceRender" J.nullRef

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies html React of any state changes.
    -- runEffect will only stop if input is finished (which in this example never does).
    void . P.runEffect $ appEffect currentState output input

    -- Cleanup
    -- We actually never get here because in this example runEffect never quits
    -- but in other apps, gadgetEffect might be quit-able (eg with MaybeT)
    -- so let's add the cleanup code here to be explicit.
    J.releaseCallback inputChangeFirer'
    J.releaseCallback inputSubmitFirer'
    J.releaseCallback toggleCompleteAllFirer'

foreign import javascript unsafe
  "hgr$todo$registry['listen']($1, $2);"
  js_globalListen :: J.JSString -> J.Callback a -> IO ()

foreign import javascript unsafe
  "hgr$todo$registry['shout']($1, $2);"
  js_globalShout :: J.JSString -> J.JSVal -> IO ()

mkActionCallback
    :: PC.Output TD.App.Action
    -> (J.JSVal -> MaybeT IO TD.App.Action)
    -> IO (J.Callback (J.JSVal -> IO ()))
mkActionCallback output handler =
    J.syncCallback1 J.ContinueAsync $ \evt ->
        void $ runMaybeT $ do
            action <- handler evt
            lift $ void $ atomically $ PC.send output action

forceRender :: (MonadIO io, MonadState TD.App.Model io) => MVar TD.App.Model -> io ()
forceRender stateMVar = do
    s <- get
    liftIO . void $ swapMVar stateMVar s -- ^ so that the render callback can use the latest state
    liftIO $ js_globalShout "forceRender" J.nullRef -- ^ tell React to call render

interpretCommand :: (MonadIO io, MonadState TD.App.Model io) => MVar TD.App.Model -> PC.Output TD.App.Action -> TD.App.Command -> io ()
interpretCommand stateMVar _ TD.App.StateChangedCommand = forceRender stateMVar

interpretCommand stateMVar _ (TD.App.RunCommand actions) = liftIO $ actions

interpretCommand stateMVar _ (TD.App.InputCommand (TD.Input.StateChangedCommand)) = forceRender stateMVar

interpretCommand _ output (TD.App.InputCommand (TD.Input.SubmitCommand str)) =
    liftIO $ void $ atomically $ PC.send output (TD.App.NewTodoAction str)

interpretCommand stateMVar _ (TD.App.TodosCommand (_, TD.Todo.StateChangedCommand)) = forceRender stateMVar

interpretCommand _ output (TD.App.TodosCommand (k, TD.Todo.DestroyCommand)) =
    liftIO $ void $ atomically $ PC.send output (TD.App.DestroyTodoAction k)

interpretCommands
    :: (Foldable t, MonadState TD.App.Model io, MonadIO io)
    => MVar TD.App.Model -> PC.Output TD.App.Action -> t TD.App.Command -> io ()
interpretCommands stateMVar output = traverse_ (interpretCommand stateMVar output)

interpretCommandsPipe
    :: (MonadState TD.App.Model io, MonadIO io)
    => MVar TD.App.Model -> PC.Output TD.App.Action -> P.Pipe (D.DList TD.App.Command) () io ()
interpretCommandsPipe stateMVar output = PP.mapM go
   where
     isStateChangedCmd cmd = case cmd of
         TD.App.StateChangedCommand -> True
         TD.App.InputCommand TD.Input.StateChangedCommand -> True
         TD.App.TodosCommand (_, TD.Todo.StateChangedCommand) -> True
         _ -> False
     go cmds = do
         let (changes, cmds') = partition isStateChangedCmd (D.toList cmds)
         interpretCommands stateMVar output cmds'
         -- Run only one of the state changed
         interpretCommands stateMVar output (foldMap (First . Just) changes)

appEffect :: MonadIO io => MVar TD.App.Model -> PC.Output TD.App.Action -> PC.Input TD.App.Action -> P.Effect io TD.App.Model
appEffect stateMVar output input = do
    s <- liftIO $ readMVar stateMVar
    PL.execStateP s $ TD.App.producer input P.>-> interpretCommandsPipe stateMVar output P.>-> PP.drain
