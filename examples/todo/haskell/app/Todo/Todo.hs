{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Todo.Todo
    ( Command(..)
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

import Control.Monad.Free.Church
import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import Data.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Extras as E
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Nullable as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Util as R
import qualified Todo.Widget as TD

data Command
    -- Common widget commands
    = RenderCommand SuperModel [E.Property] J.JSVal
    | SetPropertyCommand E.Property J.JSVal
    -- widget specific commands
    | DestroyCommand
    | FocusNodeCommand J.JSVal

data Action
    -- Common widget actions
    = ComponentRefAction J.JSVal
    | ComponentDidUpdateAction
    | SendCommandAction Command
    -- widget specific actions
    | EditRefAction J.JSVal
    | StartEditAction
    | ToggleCompletedAction
    | SetCompletedAction Bool
    | DestroyAction
    | CancelEditAction
    | SubmitAction J.JSString

data Model = Model
    -- common widget model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _deferredCommands :: D.DList Command
    -- widget specifc model
    , _editRef :: J.JSVal
    , _value :: J.JSString
    , _completed :: Bool
    , _editing :: Bool
    }

data Callbacks = Callbacks
    -- common widget callbacks
    { _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    -- widget specific callbacks
    , _onEditRef :: J.Callback (J.JSVal -> IO ())
    , _fireToggleComplete :: J.Callback (J.JSVal -> IO ())
    , _fireStartEdit :: J.Callback (J.JSVal -> IO ())
    , _fireDestroy :: J.Callback (J.JSVal -> IO ())
    , _fireCancelEdit :: J.Callback (J.JSVal -> IO ())
    , _onEditKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving G.Generic

-- | Callbacks and pure state
type CModel = (Callbacks, Model)

-- | Mutable model for rendering callback
type MModel = MVar CModel

-- | Contains MModel and CModel
type SuperModel = (MModel, CModel)

makeClassyPrisms ''Action

instance CD.Disposing Callbacks

makeClassy ''Callbacks

makeClassy ''Model

-- | This might be different per widget
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

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
    <$> (R.mkRenderer ms $ const render)
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)
    -- widget specific callbacks
    <*> (R.mkHandler $ pure . pure . EditRefAction)
    <*> (R.mkHandler $ pure . pure . const ToggleCompletedAction)
    <*> (R.mkHandler $ pure . pure . const StartEditAction)
    <*> (R.mkHandler $ pure . pure . const DestroyAction)
    <*> (R.mkHandler $ pure . pure . const CancelEditAction)
    <*> (R.mkHandler onEditKeyDown')

mkSuperModel :: Model -> F (R.Maker Action) SuperModel
mkSuperModel s = R.mkSuperModel mkCallbacks $ \cbs -> (cbs, s)

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf R.shimComponent
        [ ("key",  s ^. model . uid . to J.jsval)
        , ("render", s ^. callbacks . onRender . to E.PureJSVal . to J.pToJSVal)
        , ("ref", s ^. callbacks . onComponentRef . to E.PureJSVal . to J.pToJSVal)
        , ("componentDidUpdate", s ^. callbacks . onComponentDidUpdate . to E.PureJSVal . to J.pToJSVal)
        ]

render :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.bh (E.strval "li") [("className", cns s)] $ do
        R.bh (E.strval "div") [ ("key", E.strval "view")
                              , ("className", E.strval "view")
                              ] $ do
            R.lf (E.strval "input") [ ("key", E.strval "toggle")
                                    , ("className", E.strval "toggle")
                                    , ("type", E.strval "checkbox")
                                    , ("checked", s ^. model . completed . to J.pToJSVal)
                                    , ("onChange", s ^. callbacks . fireToggleComplete . to J.jsval)
                                    ]
            R.bh (E.strval "label")  [ ("key", E.strval "label")
                                     , ("onDoubleClick", s ^. callbacks . fireStartEdit. to J.jsval)
                                     ] (s ^. model . value . to R.txt)
            R.lf (E.strval "button") [ ("key", E.strval "destroy")
                                     , ("className", E.strval "destroy")
                                     , ("onClick", s ^. callbacks . fireDestroy . to J.jsval)
                                     ]
        -- For uncontrolled components, we need to generate a new key per render
        -- in for react to use the new defaultValue
        R.lf (E.strval "input") [ ("key", s ^. model . frameNum . to show . to J.pack . to J.jsval)
                                , ("ref", s ^. callbacks . onEditRef . to J.jsval)
                                , ("className", E.strval "edit")
                                , ("defaultValue", s ^. model . value . to J.jsval)
                                , ("checked", s ^. model . completed . to J.pToJSVal)
                                , ("onBlur", s ^. callbacks . fireCancelEdit . to J.jsval)
                                , ("onKeyDown", s ^. callbacks . onEditKeyDown . to J.jsval)
                                ]
  where
    cns s = R.classNames [("completed", s ^. completed), ("editing", s ^. editing)]

onEditKeyDown' :: J.JSVal -> MaybeT IO [Action]
onEditKeyDown' = R.eventHandlerM TD.onInputKeyDown goLazy
  where
    goLazy :: (Maybe J.JSString, J.JSVal) -> MaybeT IO [Action]
    goLazy (ms, j) = pure $
        (SendCommandAction $ SetPropertyCommand ("value", J.pToJSVal J.empty) j)
        : maybe [CancelEditAction] (pure . SubmitAction) ms

gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
gadget = do
    a <- ask
    case a of
        -- common widget actions

        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        ComponentDidUpdateAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            cmds <- use deferredCommands
            deferredCommands .= mempty
            pure cmds

        SendCommandAction cmd -> pure $ D.singleton cmd

        -- widget specific actions
        EditRefAction v -> do
            editRef .= v
            pure mempty

        ToggleCompletedAction -> do
            completed %= not
            D.singleton <$> renderCmd

        SetCompletedAction b -> do
            completed .= b
            D.singleton <$> renderCmd

        StartEditAction -> do
            input <- use editRef
            ret <- runMaybeT $ do
                input' <- MaybeT $ pure $ J.nullableToMaybe (J.Nullable input)
                editing .= True
                -- Need to delay focusing until after the next render
                deferredCommands %= (`D.snoc` FocusNodeCommand input')
                lift $ D.singleton <$> renderCmd
            maybe (pure mempty) pure ret

        DestroyAction -> pure $ D.singleton DestroyCommand

        CancelEditAction -> do
            editing .= False
            D.singleton <$> renderCmd

        SubmitAction v -> do
            -- trim the text
            let v' = J.strip v
            value .= v'
            editing .= False
            if J.null v'
                then pure $ D.singleton DestroyCommand
                else D.singleton <$> renderCmd

renderCmd :: Monad m => G.GadgetT Action SuperModel m Command
renderCmd = do
    model . frameNum %= (+ 1)
    i <- J.pToJSVal <$> use (model . frameNum)
    r <- use (model . componentRef)
    sm <- use superModel
    pure $ RenderCommand sm [("frameNum", i)] r
