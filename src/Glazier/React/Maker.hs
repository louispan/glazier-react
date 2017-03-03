{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Glazier.React.Maker where

import Control.Monad.Free.Class
import Control.Monad.Free.TH
import Control.Concurrent.MVar
import Control.Monad.Trans.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Markup as R

-- | DSL for IO effect required during making widgets
data Maker mdl act nxt
    = MkHandler (J.JSVal -> MaybeT IO act) (J.Callback (J.JSVal -> IO ()) -> nxt)
    | MkModelMVar (MVar mdl -> nxt)
    | MkRenderer (MVar mdl) (G.WindowT mdl (R.ReactMl) ()) (J.Callback (IO J.JSVal) -> nxt)
    | PutModelMVar (MVar mdl) mdl nxt
    deriving (Functor)

makeFree ''Maker

-- | Allows changing the action type of Maker
mapAction :: (act -> act') -> Maker mdl act a -> Maker mdl act' a
mapAction f (MkHandler handler g) = MkHandler (\v -> f <$> handler v) g
mapAction _ (MkModelMVar g) = MkModelMVar g
mapAction _ (MkRenderer ms render g) = MkRenderer ms render g
mapAction _ (PutModelMVar ms s g) = PutModelMVar ms s g

mkMModel :: MonadFree (Maker mdl act) m => (MVar mdl -> m cbs) -> (cbs -> mdl) -> m (MVar mdl, mdl)
mkMModel makeCallbacks createModel = do
    ms <- mkModelMVar
    cbs <- makeCallbacks ms
    let s = createModel cbs
    putModelMVar ms s
    pure (ms, s)
