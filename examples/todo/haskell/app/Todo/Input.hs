{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Input
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Callbacks(..)
    , HasCallbacks(..)
    , mkCallbacks
    , Model(..)
    , HasModel(..)
    , CModel
    , MModel
    , SuperModel
    , mkSuperModel
    , window
    , gadget
    ) where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Extras as E
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model.Class as R
import qualified Todo.Gadget as TD

data Command
    -- Common widget commands
    = SetPropertyCommand E.Property J.JSVal
    -- widget specific commands
    | SubmitCommand J.JSString

data Action
    -- Common widget actions
    = SendCommandsAction [Command]
    -- widget specific actions
    | SubmitAction J.JSString

data Model = Model
    -- common widget model
    { _uid :: J.JSString
    -- widget specifc model
    , _placeholder :: J.JSString
    }

data Callbacks = Callbacks
    -- common widget callbacks
    { _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    -- widget specific callbacks
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

----------------------------------------------------------
-- The following should be the same per widget
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
-- CModel
instance R.HasCModel CModel CModel where
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
-- MModel
instance R.HasMModel MModel CModel where
    mModel = id
-- SuperModel
instance R.HasMModel SuperModel CModel where
    mModel = _1
instance R.HasCModel SuperModel CModel where
    cModel = _2
instance HasCallbacks SuperModel where
    callbacks = R.cModel . callbacks
instance HasModel SuperModel where
    model = R.cModel . model
instance CD.Disposing SuperModel where
    disposing s = CD.DisposeList
        [ s ^. callbacks . to CD.disposing
        , s ^. model . to CD.disposing
        ]
-- End same code per widget
----------------------------------------------------------

-- | This might be different per widget
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

mkSuperModel :: Model -> F (R.Maker Action) SuperModel
mkSuperModel s = R.mkSuperModel mkCallbacks $ \cbs -> (cbs, s)
-- End similar code per widget
----------------------------------------------------------

mkCallbacks :: MVar CModel -> F (R.Maker Action) Callbacks
mkCallbacks ms = Callbacks
    -- common widget callbacks
    <$> (R.mkRenderer ms $ const render)
    -- widget specific callbacks
    <*> (R.mkHandler onKeyDown')

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf R.shimComponent
        [ ("key",  s ^. uid . to J.jsval)
        , ("render", s ^. onRender . to E.PureJSVal . to J.pToJSVal)
        ]

-- | This is used by the React render callback
render :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.lf (E.strval "input")
                    [ ("key", s ^. uid . to J.jsval)
                    , ("className", E.strval "new-todo")
                    , ("placeholder", s ^. placeholder . to J.jsval)
                    , ("autoFocus", J.pToJSVal True)
                    , ("onKeyDown", s ^. onKeyDown . to J.jsval)
                    ]

onKeyDown' :: J.JSVal -> MaybeT IO [Action]
onKeyDown' = R.eventHandlerM TD.onInputKeyDown goLazy
  where
    goLazy :: (Maybe J.JSString, J.JSVal) -> MaybeT IO [Action]
    goLazy (ms, j) = pure $
        SendCommandsAction [SetPropertyCommand ("value", J.pToJSVal J.empty) j]
        : maybe [] (pure . SubmitAction) ms

-- | State update logic.
-- The best practice is to leave this in general Monad m (eg, not MonadIO).
-- This allows gadget to use STM as the base monad which allows for combining concurrently
-- with other stateful STM effects and still maintain a single source of truth.
gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
gadget = do
    a <- ask
    case a of
        -- common widget actions
        SendCommandsAction cmds -> pure $ D.fromList cmds

        -- widget specific actions
        SubmitAction v -> do
            let v' = J.strip v
            if J.null v'
                then pure mempty
                else pure $ D.singleton $ SubmitCommand v'
