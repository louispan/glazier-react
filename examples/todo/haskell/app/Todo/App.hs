{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Todo.App
    ( TodosKey
    , TodosValue
    , TodosCommand'
    , TodosAction'
    , TodosModel'
    , Command(..)
    , Action(..)
    , AsAction(..)
    , Callbacks(..)
    , HasCallbacks(..)
    , mkCallbacks
    , Model(..)
    , HasModel(..)
    , CModel
    , MModel
    , mkMModel
    , window
    , gadget
    ) where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
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

type TodosValue = TD.Todo.MModel

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
    { _onRender :: J.Callback (IO J.JSVal)
    , _onRef :: J.Callback (J.JSVal -> IO ())
    , _onUpdated :: J.Callback (J.JSVal -> IO ())
    -- TodoMVC specific callbacks
    , _fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

instance CD.Disposing Callbacks

makeClassy ''Callbacks

data Model = Model
    -- Common widget model
    { _uid :: J.JSString
    , _ref :: J.JSVal -- ^ ref to react component object
    , _frameNum :: FrameNum -- | Current rendered frame seqNum
    , _deferredCommands :: M.Map FrameNum (D.DList Command)
    -- TodoMVC specifc model
    , _todoSeqNum :: TodosKey
    , _todoInput :: (MVar TD.Input.CModel, TD.Input.CModel)
    , _todosModel :: TodosModel'
    }

makeClassy ''Model

type CModel = (Callbacks, Model)

-- | This might be different per widget
instance CD.Disposing Model where
    disposing s = CD.DisposeList $
        CD.disposing (s ^. todoInput)
        : s ^. todosModel . to (fmap (CD.disposing . snd) . M.toList)

instance CD.Disposing CModel where
    disposing s = CD.DisposeList
        [ s ^. callbacks . to CD.disposing
        , s ^. model . to CD.disposing
        ]

instance HasCallbacks CModel where
    callbacks = _1

instance HasModel CModel where
    model = _2

type MModel = (MVar CModel, CModel)

instance CD.Disposing MModel where
    disposing s = CD.DisposeList
        [ s ^. callbacks . to CD.disposing
        , s ^. model . to CD.disposing
        ]

instance HasCallbacks MModel where
    callbacks = _2 . callbacks

instance HasModel MModel where
    model = _2 . model

mkCallbacks :: MVar CModel -> F (R.Maker Action) Callbacks
mkCallbacks ms = Callbacks
    -- common widget callbacks
    <$> (R.mkRenderer ms render)
    <*> (R.mkHandler $ R.onRef RefAction)
    <*> (R.mkHandler $ R.onUpdated RenderedAction)
    -- widget specific callbacks
    <*> (R.mkHandler fireToggleCompleteAll')

mkMModel :: J.JSString -> F (R.Maker Action) MModel
mkMModel uid' = do
    minput <- hoistF (R.mapAction $ review _InputAction) $
        TD.Input.mkMModel $ TD.Input.Model
            "newtodo"
            J.nullRef
            0
            mempty
            "What needs to be done?"
            J.empty
    R.mkMModel mkCallbacks $ \cbs -> (cbs, Model
        uid'
        J.nullRef
        0
        mempty
        0
        minput
        mempty)

hasActiveTodos :: TodosModel' -> Bool
hasActiveTodos = not . null . filter (not . TD.Todo._completed) . fmap (view TD.Todo.model . snd) . M.toList

fireToggleCompleteAll' :: Applicative m => J.JSVal -> m Action
fireToggleCompleteAll' = const $ pure ToggleCompleteAllAction

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf R.shimComponent
        [ ("key",  s ^. model . uid . to J.jsval)
        , ("render", s ^. callbacks . onRender . to E.PureJSVal . to J.pToJSVal)
        , ("ref", s ^. callbacks . onRef . to E.PureJSVal . to J.pToJSVal)
        , ("updated", s ^. callbacks . onUpdated . to E.PureJSVal . to J.pToJSVal)
        ]

-- | This is used by the React render callback
render :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.bh (E.strval "header") [("className", E.strval "header")] $ do
        R.bh (E.strval "h1") [("key", E.strval "heading")] (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT mainWindow s

mainWindow :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
mainWindow = do
    todos <- view todosModel
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
                        , ("checked", s ^. todosModel . to (J.pToJSVal . not . hasActiveTodos))
                        , ("onChange", s ^. callbacks . fireToggleCompleteAll . to J.jsval)
                        ]
            view G._WindowT todoListWindow s

todoListWindow :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
todoListWindow = do
    todos <- fmap (snd . snd) . M.toList <$> view todosModel -- FIXME: HasCModel ?!
    lift $ R.bh (E.strval "ul") [ ("key", E.strval "todo-list")
                                , ("className", E.strval "todo-list")
                                ] $
        traverse_ (view G._WindowT TD.Todo.window) todos

gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = appGadget
    <> inputGadget
    <> todosGadget'

appGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
appGadget = do
    a <- ask
    case a of
        RefAction node -> do
            ref .= node
            pure mempty

        RenderedAction n -> do
            -- Run delayed commands that need to wait until a particular frame is rendered
            -- Eg focusing after other rendering changes
            -- All deferred commands with renderSeqNum lower than the rendered SeqNum is
            -- safe to run
            cmds <- use deferredCommands
            let (cmds', leftoverCmds) = M.partitionWithKey (\k _ -> k < n) cmds
            deferredCommands .= leftoverCmds
            pure . foldMap snd . M.toList $ cmds'

        ToggleCompleteAllAction -> do
            s <- use todosModel
            let b = hasActiveTodos s
                (cmds, s') = M.mapAccumWithKey (toggleCompleteAll b) mempty s
            todosModel .= s'
            pure cmds

        DestroyTodoAction k -> do
            -- queue up callbacks to be released after rerendering
            ts <- use todosModel
            ret <- runMaybeT $ do
                (_, todoModel) <- MaybeT $ pure $ M.lookup k ts
                let junk = CD.disposing todoModel
                i <- use frameNum
                deferredCommands %= (M.alter (addCommand $ DisposeCommand junk) i)
                -- Remove the todo from the model
                todosModel %= M.delete k
                -- on re-render the todo Shim will not get rendered and will be removed by react
                pure $ D.singleton RenderCommand
            maybe (pure mempty) pure ret

        RequestNewTodoAction str -> do
            n <- use todoSeqNum
            todoSeqNum %= (+ 1)
            pure $ D.singleton $ MakerCommand $ do
                (ms, s) <- hoistF (R.mapAction $ \act -> TodosAction (n, act)) $
                    TD.Todo.mkMModel $ TD.Todo.Model
                        (J.pack . show $ n)
                        J.nullRef
                        0
                        mempty
                        J.nullRef
                        str
                        False
                        Nothing
                pure $ AddNewTodoAction n (ms, s)

        AddNewTodoAction n v -> do
            todosModel %= M.insert n v
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
        -> (a, TD.Todo.CModel)
        -> (D.DList Command, (a, TD.Todo.CModel))
    toggleCompleteAll b cmds k (a, s) =
        if (s ^. (TD.Todo.model . TD.Todo.completed) /= b)
            then ( cmds `D.snoc` TodosCommand (k, TD.Todo.RenderCommand)
                 , (a, s & TD.Todo.model . TD.Todo.completed .~ b))
            else (cmds, (a, s))

inputWindow :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
inputWindow = magnify (todoInput . _2) TD.Input.window -- HasCModel

inputGadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
inputGadget = fmap InputCommand <$> zoom (todoInput . TD.Input.model) (magnify _InputAction TD.Input.gadget)

todosGadget :: Monad m => G.GadgetT TodosAction' TodosModel' m (D.DList TodosCommand')
todosGadget = do
    -- expect a (key, action) pair
    (k, a) <- ask
    x <- get
    case M.lookup k x of
        Nothing -> pure mempty
        Just (mcs, cs) -> do
            -- run the todo gadget logic
            let s = cs ^. TD.Todo.model
            (cmds, s') <- lift $ view G._GadgetT TD.Todo.gadget a s
            let cs' = cs & TD.Todo.model .~ s'
            -- replace the todo state in the map
            put $ M.insert k (mcs, cs') x
            -- annotate cmd with the key
            pure $ (\cmd -> (k, cmd)) <$> cmds

todosGadget' :: Monad m => G.GadgetT Action Model m (D.DList Command)
todosGadget' = fmap TodosCommand <$> zoom todosModel (magnify _TodosAction todosGadget)
