{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Component.Display
    ( Plan(..)
    , HasPlan(..)
    , Display
    , display
    ) where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

data Plan = Plan
    { _component :: R.ReactComponent
    , _key :: J.JSString
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan
    :: G.WindowT mdl R.ReactMl () -> MVar mdl -> F (R.Maker a) Plan
mkPlan render frm = Plan
    <$> R.getComponent
    <*> R.mkKey
    <*> (R.mkRenderer frm $ const render)

instance CD.Disposing Plan

instance HasPlan pln => HasPlan (R.Model dtl pln) where
    plan = R.plan . plan

-- | Undecidableinstances! This is safe because pln is smaller than mdl
instance (R.HasModel mdl dtl pln, HasPlan pln) => HasPlan (R.Shared mdl) where
    plan = R.plan . plan

-- | Exposed to parent components to render this component
window :: (R.HasModel mdl dtl pln, HasPlan pln) => (mdl -> R.WindowAttrs) -> G.WindowT mdl R.ReactMl ()
window fattrs = do
    s <- ask
    let R.WindowAttrs (props, hdls) = fattrs s
    lift $
        R.lf
            (s ^. R.model . component . to JE.toJS')
            ([ ("key", s ^. R.model . key . to JE.toJS')
             -- NB. render is not a 'Handle' as it returns an 'IO JSVal'
             , ("render", s ^. R.model . onRender . to JE.toJS')
             ] ++
             props)
            hdls

type Display a mdl = R.Display a Plan mdl

display :: (HasPlan pln, R.HasModel mdl dtl pln) => G.WindowT mdl R.ReactMl ()
     -> (mdl -> R.WindowAttrs) -> R.Display a Plan mdl
display render fattrs = R.Display (mkPlan render) (window fattrs)
