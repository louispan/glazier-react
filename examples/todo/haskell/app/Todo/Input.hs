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
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Extras as E
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R

data Command
    -- Common widget commands
    = RenderCommand [E.Property] J.JSVal
    -- widget specific commands
    | SubmitCommand J.JSString

data Action
    -- Common widget actions
    = RefAction J.JSVal
    -- widget specific actions
    | SubmitAction J.JSString

makeClassyPrisms ''Action

data Callbacks = Callbacks
    -- common widget callbacks
    { _onRender :: J.Callback (J.JSVal ->
                               IO J.JSVal)
    , _onRef :: J.Callback (J.JSVal -> IO ())
    -- widget specific callbacks
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

instance CD.Disposing Callbacks

makeClassy ''Callbacks

data Model = Model
    -- common widget model
    { _uid :: J.JSString
    , _ref :: J.JSVal -- ^ ref to react component object
    , _frameNum :: Int
    -- widget specifc model
    , _placeholder :: J.JSString
    , _defaultValue :: J.JSString
    }

makeClassy ''Model

-- | This might be different per widget
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

-- | Callbacks and pure state
type CModel = (Callbacks, Model)

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

-- | Mutable model for rendering callback
type MModel = MVar CModel

class HasMModel s where
    mModel :: Lens' s MModel

instance HasMModel MModel where
    mModel = id

-- | Contains MModel and CModel
type SuperModel = (MModel, CModel)

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
    <*> (R.mkHandler $ R.onRef RefAction)
    -- widget specific callbacks
    <*> (R.mkHandler onKeyDown')

mkSuperModel :: Model -> F (R.Maker Action) SuperModel
mkSuperModel s = R.mkSuperModel mkCallbacks $ \cbs -> (cbs, s)

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf R.shimComponent
        [ ("key",  s ^. model . uid . to J.jsval)
        , ("render", s ^. callbacks . onRender . to E.PureJSVal . to J.pToJSVal)
        , ("ref", s ^. callbacks . onRef . to E.PureJSVal . to J.pToJSVal)
        ]

-- | This is used by the React render callback
render :: Monad m => G.WindowT CModel (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.lf (E.strval "input")
                    [ ("className", E.strval "new-todo")
                    , ("placeholder", s ^. model . placeholder . to J.jsval)
                    , ("defaultValue", s ^. model . defaultValue . to J.jsval)
                    , ("autoFocus", J.pToJSVal True)
                    , ("onKeyDown", s ^. callbacks . onKeyDown . to J.jsval)
                    ]

foreign import javascript unsafe
  "if ($1 && $1['value']) { $1['value'] = ''; }"
  js_resetValue :: J.JSVal -> IO ()

onKeyDown' :: J.JSVal -> MaybeT IO Action
onKeyDown' = R.eventHandlerM goStrict goLazy
  where
    goStrict :: J.JSVal -> MaybeT IO J.JSString
    goStrict evt = do
        evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
        evt'' <- MaybeT $ pure $ R.parseKeyboardEvent evt'
        let k = R.keyCode evt''
        guard (k == 13) -- FIXME: ENTER_KEY
        -- target is the "input" DOM
        input <- lift $ pure . J.jsval . R.target . R.parseEvent $ evt'
        v <- MaybeT $ E.getProperty "value" input >>= J.fromJSVal
        lift $ js_resetValue input
        pure v

    goLazy :: J.JSString -> MaybeT IO Action
    goLazy mv = pure $ SubmitAction mv


-- | State update logic.
-- The best practice is to leave this in general Monad m (eg, not MonadIO).
-- This allows gadget to use STM as the base monad which allows for combining concurrently
-- with other stateful STM effects and still maintain a single source of truth.
gadget :: Monad m => G.GadgetT Action Model m (D.DList Command)
gadget = do
    a <- ask
    case a of
        -- common widget actions
        RefAction node -> do
            ref .= node
            pure mempty

        -- widget specific actions
        SubmitAction v -> do
            let v' = J.strip v
            if J.null v'
                then pure mempty
                else do
                    frameNum %= (+ 1)
                    i <- J.pToJSVal <$> use frameNum
                    r <- use ref
                    pure (D.fromList [SubmitCommand v, RenderCommand [("frameNum", i)] r])
