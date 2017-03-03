{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.App
    ( TodosKey
    , TodosValue
    , TodosCommand'
    , TodosAction'
    , TodosModel'
    , Callbacks(..)
    , Command(..)
    , Action(..)
    , AsAction(..)
    , Model(..)
    , HasModel(..)
    , window
    , gadget
    , mkCallbacks
    , mkMModel
    ) where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Free.Orphans
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Extras as E
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Maker as R
import qualified Todo.Input as TD.Input
import qualified Todo.Todo as TD.Todo

type TodosKey = Int

type TodosValue = (MVar TD.Todo.Model, TD.Todo.Model)

type TodosModel' = M.Map TodosKey TodosValue

type TodosCommand' = (TodosKey, TD.Todo.Command)

type TodosAction' = (TodosKey, TD.Todo.Action)

type FrameNum = Int

data Command
    -- Common widget commands
    -- | This should result in React component @setState({ frameNum: i })@
    = RenderCommand

    -- General Application level commands
    -- | DisposeCommand should run dispose on the SomeDisposable (eg. to release Callbacks)
    | DisposeCommand CD.SomeDisposable
    -- | Runs maker widgets creations
    | MakerCommand (F (R.Maker Action) Action)

    -- TodoMVC specific commands
    | InputCommand TD.Input.Command
    | TodosCommand TodosCommand'

data Action
    -- Common widget actions
    = RefAction J.JSVal
    | RenderedAction FrameNum
    -- TodoMVC specific actions
    | ToggleCompleteAllAction
    | DestroyTodoAction TodosKey
    | RequestNewTodoAction J.JSString
    | AddNewTodoAction TodosKey TodosValue
    | InputAction TD.Input.Action
    | TodosAction TodosAction'

makeClassyPrisms ''Action

data Callbacks = Callbacks
    -- Common widget callbacks
    { onRender :: J.Callback (IO J.JSVal)
    , onRef :: J.Callback (J.JSVal -> IO ())
    , onUpdated :: J.Callback (J.JSVal -> IO ())
    -- TodoMVC specific callbacks
    , fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

instance CD.Disposing Callbacks

data Model = Model
    -- Common widget model
    { callbacks :: Callbacks
    , uid :: J.JSString
    , ref :: J.JSVal -- ^ ref to react component object
    , frameNum :: FrameNum -- | Current rendered frame seqNum
    , deferredCommands :: M.Map FrameNum (D.DList Command)
    -- TodoMVC specifc model
    , todoSeqNum :: TodosKey
    , todoInput :: (MVar TD.Input.Model, TD.Input.Model)
    , todosModel :: TodosModel'
    }

makeClassy_ ''Model

mkCallbacks :: MVar Model -> F (R.Maker Action) Callbacks
mkCallbacks ms = Callbacks
    -- common widget callbacks
    <$> (R.mkRenderer ms render)
    <*> (R.mkHandler $ R.onRef RefAction)
    <*> (R.mkHandler $ R.onUpdated RenderedAction)
    -- widget specific callbacks
    <*> (R.mkHandler fireToggleCompleteAll')

instance CD.Disposing Model where
    disposing s = CD.DisposeList [ CD.disposing $ callbacks s
                                 , CD.disposing $ snd $ todoInput s
                                 ]

mkMModel :: MonadFree (R.Maker Action) m => J.JSString -> m (MVar Model, Model)
mkMModel uid' = do
    (minput, input) <- hoist (R.mapAction $ review _InputAction) $
        TD.Input.mkMModel "newtodo"
    R.mkMModel mkCallbacks $
        \cbs -> Model
                cbs
                uid'
                J.nullRef
                0
                mempty
                0
                (minput, input)
                mempty

hasActiveTodos :: TodosModel' -> Bool
hasActiveTodos = not . null . filter (not . TD.Todo.completed) . fmap snd . fmap snd . M.toList

fireToggleCompleteAll' :: Applicative m => J.JSVal -> m Action
fireToggleCompleteAll' = const $ pure ToggleCompleteAllAction

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT Model (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf R.shimComponent
        [ ("key",  J.jsval $ uid s)
        , ("render", J.pToJSVal . E.PureJSVal . onRender . callbacks $ s)
        , ("ref", J.pToJSVal . E.PureJSVal . onRef . callbacks $ s)
        , ("updated", J.pToJSVal . E.PureJSVal . onUpdated . callbacks $ s) ]

-- | This is used by the React render callback
render :: Monad m => G.WindowT Model (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.bh (E.strval "header") [("className", E.strval "header")] $ do
        R.bh (E.strval "h1") [("key", E.strval "heading")] (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT mainWindow s

mainWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
mainWindow = do
    todos <- view _todosModel
    if (null todos)
        then pure ()
        else do
        s <- ask
        lift $ R.bh (E.strval "section") [ ("key", E.strval "main")
                                         , ("className", E.strval "main")
                                         ] $ do
            -- This is the complete all checkbox
            R.lf (E.strval "input")
                        [ ("key", E.strval "toggle-all")
                        , ("className", E.strval "toggle-all")
                        , ("type", E.strval "checkbox")
                        , ("checked", J.pToJSVal . not . hasActiveTodos $ todos)
                        , ("onChange", J.jsval $ fireToggleCompleteAll $ callbacks s)
                        ]
            view G._WindowT todoListWindow s

todoListWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
todoListWindow = do
    todos <- fmap snd . M.toList <$> view _todosModel
    lift $ R.bh (E.strval "ul") [ ("key", E.strval "todo-list")
                                , ("className", E.strval "todo-list")
                                ] $
        traverse_ (view G._WindowT TD.Todo.window . snd) todos

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = appGadget
    <> inputGadget
    <> todosGadget'

appGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
appGadget = do
    a <- ask
    case a of
        RefAction node -> do
            _ref .= node
            pure mempty

        RenderedAction n -> do
            -- Run delayed commands that need to wait until a particular frame is rendered
            -- Eg focusing after other rendering changes
            -- All deferred commands with renderSeqNum lower than the rendered SeqNum is
            -- safe to run
            cmds <- use _deferredCommands
            let (cmds', leftoverCmds) = M.partitionWithKey (\k _ -> k < n) cmds
            _deferredCommands .= leftoverCmds
            pure . foldMap snd . M.toList $ cmds'

        ToggleCompleteAllAction -> do
            s <- use _todosModel
            let b = hasActiveTodos s
                (cmds, s') = M.mapAccumWithKey (toggleCompleteAll b) mempty s
            _todosModel .= s'
            pure cmds

        DestroyTodoAction k -> do
            -- queue up callbacks to be released after rerendering
            ts <- use _todosModel
            ret <- runMaybeT $ do
                (_, todoModel) <- MaybeT $ pure $ M.lookup k ts
                let junk = CD.disposing todoModel
                i <- use _frameNum
                _deferredCommands %= (M.alter (addCommand $ DisposeCommand junk) i)
                -- Remove the todo from the model
                _todosModel %= M.delete k
                -- on re-render the todo Shim will not get rendered and will be removed by react
                pure $ D.singleton RenderCommand
            maybe (pure mempty) pure ret

        RequestNewTodoAction str -> do
            n <- use _todoSeqNum
            _todoSeqNum %= (+ 1)
            pure $ D.singleton $ MakerCommand $ do
                (ms, s) <- hoist (R.mapAction $ \act -> TodosAction (n, act)) $
                    TD.Todo.mkMModel (J.pack . show $ n) str
                pure $ AddNewTodoAction n (ms, s)

        AddNewTodoAction n v -> do
            _todosModel %= M.insert n v
            pure $ D.singleton RenderCommand

        -- these will be handled by monoidally appending other gadgets
        InputAction _ -> pure mempty
        TodosAction _ -> pure mempty
  where
    addCommand cmd Nothing = Just $ D.singleton cmd
    addCommand cmd (Just cmds') = Just (cmds' `D.snoc` cmd)

    toggleCompleteAll
        :: Bool
        -> D.DList Command
        -> TodosKey
        -> (a, TD.Todo.Model)
        -> (D.DList Command, (a, TD.Todo.Model))
    toggleCompleteAll b cmds k (a, s) =
        if (TD.Todo.completed s /= b)
            then ( cmds `D.snoc` TodosCommand (k, TD.Todo.RenderCommand)
                 , (a, s & TD.Todo._completed .~ b))
            else (cmds, (a, s))

inputWindow :: Monad m => G.WindowT Model (R.ReactMlT m) ()
inputWindow = magnify (_todoInput . _2) TD.Input.window

inputGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
inputGadget = fmap InputCommand <$> zoom (_todoInput . _2) (magnify _InputAction TD.Input.gadget)

todosGadget :: Monad m => G.GadgetT TodosAction' TodosModel' m (D.DList TodosCommand')
todosGadget = do
    -- expect a (key, action) pair
    (k, a) <- ask
    s <- get
    case M.lookup k s of
        Nothing -> pure mempty
        Just (ms, s') -> do
            -- run the todo gadget logic
            (cmds, s'') <- lift $ view G._GadgetT TD.Todo.gadget a s'
            put $ M.insert k (ms, s'') s
            -- annotate cmd with the key
            pure $ (\cmd -> (k, cmd)) <$> cmds

todosGadget' :: Monad m => G.GadgetT Action Model m (D.DList Command)
todosGadget' = fmap TodosCommand <$> zoom _todosModel (magnify _TodosAction todosGadget)
