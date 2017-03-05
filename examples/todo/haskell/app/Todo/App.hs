{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
    , HasCModel(..)
    , MModel
    , HasMModel(..)
    , SuperModel
    , HasSuperModel(..)
    , mkSuperModel
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

type TodosValue = TD.Todo.SuperModel

type TodosModel' = M.Map TodosKey TodosValue

type TodosCommand' = (TodosKey, TD.Todo.Command)

type TodosAction' = (TodosKey, TD.Todo.Action)

data Command
    -- Common widget commands
    -- | This should result in React component @setState({ frameNum: i })@
    = RenderCommand SuperModel [E.Property] J.JSVal

    -- General Application level commands
    -- | DisposeCommand should run dispose on the SomeDisposable (eg. to release Callbacks)
    | DisposeCommand CD.SomeDisposable
    -- | Runs maker widgets creations
    | MakerCommand (F (R.Maker Action) Action)
    | SendActionsCommand [Action]

    -- TodoMVC specific commands
    | InputCommand TD.Input.Command
    | TodosCommand TodosCommand'

data Action
    -- Common widget actions
    = ComponentRefAction J.JSVal
    | ComponentDidUpdateAction
    -- TodoMVC specific actions
    | ToggleCompleteAllAction
    | DestroyTodoAction TodosKey
    | RequestNewTodoAction J.JSString
    | AddNewTodoAction TodosKey TodosValue
    | InputAction TD.Input.Action
    | TodosAction TodosAction'

data Model = Model
    -- Common widget model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _deferredCommands :: D.DList Command
    -- TodoMVC specifc model
    , _todoSeqNum :: TodosKey
    , _todoInput :: TD.Input.SuperModel
    , _todosModel :: TodosModel'
    }

data Callbacks = Callbacks
    -- Common widget callbacks
    { _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    -- TodoMVC specific callbacks
    , _fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

-- | Callbacks and pure state
type CModel = (Callbacks, Model)

-- | Mutable model for rendering callback
type MModel = MVar CModel

-- | Contains MModel and CModel
type SuperModel = (MModel, CModel)

makeClassyPrisms ''Action

makeClassy ''Callbacks

makeClassy ''Model

instance CD.Disposing Callbacks

-- | This might be different per widget
instance CD.Disposing Model where
    disposing s = CD.DisposeList $
        CD.disposing (s ^. todoInput)
        : foldr ((:) . CD.disposing) [] (s ^. todosModel)

class HasCModel s where
    cModel :: Lens' s CModel

instance HasCModel CModel where
    cModel = id

instance HasCallbacks CModel where
    callbacks = _1

instance HasModel CModel where
    model = _2

instance CD.Disposing CModel where
    disposing s = CD.DisposeList
        [ s ^. callbacks . to CD.disposing
        , s ^. model . to CD.disposing
        ]
class HasMModel s where
    mModel :: Lens' s MModel

instance HasMModel MModel where
    mModel = id

class HasSuperModel s where
    superModel :: Lens' s SuperModel

instance HasSuperModel SuperModel where
  superModel = id

instance HasMModel SuperModel where
    mModel = _1

instance HasCModel SuperModel where
    cModel = _2

instance HasCallbacks SuperModel where
    callbacks = cModel . callbacks

instance HasModel SuperModel where
    model = cModel . model

instance CD.Disposing SuperModel where
    disposing s = CD.DisposeList
        [ s ^. callbacks . to CD.disposing
        , s ^. model . to CD.disposing
        ]

mkCallbacks :: MVar CModel -> F (R.Maker Action) Callbacks
mkCallbacks ms = Callbacks
    -- common widget callbacks
    <$> (R.mkRenderer ms (const render))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)
    -- widget specific callbacks
    <*> (R.mkHandler $ pure . pure . const ToggleCompleteAllAction)

mkSuperModel :: J.JSString -> F (R.Maker Action) SuperModel
mkSuperModel uid' = do
    minput <- hoistF (R.mapAction $ review _InputAction) $
        TD.Input.mkSuperModel $ TD.Input.Model
            "newtodo"
            "What needs to be done?"
            J.empty
    R.mkSuperModel mkCallbacks $ \cbs -> (cbs, Model
        uid'
        J.nullRef
        0
        mempty
        0
        minput
        mempty)

hasActiveTodos :: TodosModel' -> Bool
hasActiveTodos = getAny . foldMap (view (TD.Todo.model . TD.Todo.completed . to not . to Any))

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf R.shimComponent
        [ ("key",  s ^. uid . to J.jsval)
        , ("render", s ^. onRender . to E.PureJSVal . to J.pToJSVal)
        , ("ref", s ^. onComponentRef . to E.PureJSVal . to J.pToJSVal)
        , ("componentDidUpdate", s ^. onComponentDidUpdate . to E.PureJSVal . to J.pToJSVal)
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
                        , ("onChange", s ^. fireToggleCompleteAll . to J.jsval)
                        ]
            view G._WindowT todoListWindow s

todoListWindow :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
todoListWindow = do
    todos <- fmap (view TD.Todo.cModel . snd) . M.toList <$> view todosModel
    lift $ R.bh (E.strval "ul") [ ("key", E.strval "todo-list")
                                , ("className", E.strval "todo-list")
                                ] $
        traverse_ (view G._WindowT TD.Todo.window) todos

gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
gadget = appGadget
    <> inputGadget
    <> todosGadget'

appGadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
appGadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        ComponentDidUpdateAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            cmds <- use deferredCommands
            deferredCommands .= mempty
            pure cmds

        ToggleCompleteAllAction -> do
            s <- use todosModel
            let b = hasActiveTodos s
            let acts = M.foldMapWithKey (toggleCompleteAll b) s
            pure $ D.singleton $ SendActionsCommand (D.toList acts)

        DestroyTodoAction k -> do
            -- queue up callbacks to be released after rerendering
            ts <- use todosModel
            ret <- runMaybeT $ do
                (_, todoModel) <- MaybeT $ pure $ M.lookup k ts
                let junk = CD.disposing todoModel
                deferredCommands %= (`D.snoc` DisposeCommand junk)
                -- Remove the todo from the model
                todosModel %= M.delete k
                -- on re-render the todo Shim will not get rendered and will be removed by react
                lift $ D.singleton <$> renderCmd
            maybe (pure mempty) pure ret

        RequestNewTodoAction str -> do
            n <- use todoSeqNum
            todoSeqNum %= (+ 1)
            pure $ D.singleton $ MakerCommand $ do
                ms <- hoistF (R.mapAction $ \act -> TodosAction (n, act)) $
                    TD.Todo.mkSuperModel $ TD.Todo.Model
                        (J.pack . show $ n)
                        J.nullRef
                        0
                        mempty
                        J.nullRef
                        str
                        False
                        False
                pure $ AddNewTodoAction n ms

        AddNewTodoAction n v -> do
            todosModel %= M.insert n v
            D.singleton <$> renderCmd

        -- these will be handled by monoidally appending other gadgets
        InputAction _ -> pure mempty
        TodosAction _ -> pure mempty
  where
    toggleCompleteAll
        :: Bool
        -> TodosKey
        -> TD.Todo.SuperModel
        -> D.DList Action
    toggleCompleteAll b k (_, s) =
        if (s ^. (TD.Todo.model . TD.Todo.completed) /= b)
            then D.singleton $ TodosAction (k, TD.Todo.SetCompletedAction b)
            else mempty

inputWindow :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
inputWindow = magnify (todoInput . TD.Input.cModel) TD.Input.window

inputGadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
inputGadget = fmap InputCommand <$> zoom todoInput (magnify _InputAction TD.Input.gadget)

todosGadget :: Monad m => G.GadgetT TodosAction' TodosModel' m (D.DList TodosCommand')
todosGadget = do
    -- expect a (key, action) pair
    (k, a) <- ask
    x <- get
    case M.lookup k x of
        Nothing -> pure mempty
        Just sm -> do
            -- run the todo gadget logic
            (cmds, sm') <- lift $ view G._GadgetT TD.Todo.gadget a sm
            -- replace the todo state in the map
            put $ M.insert k sm' x
            -- annotate cmd with the key
            pure $ (\cmd -> (k, cmd)) <$> cmds

todosGadget' :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
todosGadget' = fmap TodosCommand <$> zoom todosModel (magnify _TodosAction todosGadget)

-- | Just change the state to something different so the React pureComponent will call render()
renderCmd :: Monad m => G.GadgetT Action SuperModel m Command
renderCmd = do
    frameNum %= (\i -> (i + 1) `mod` 100)
    i <- J.pToJSVal <$> use frameNum
    r <- use componentRef
    sm <- use superModel
    pure $ RenderCommand sm [("frameNum", i)] r
