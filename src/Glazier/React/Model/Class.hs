{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Model.Class where

import Control.Concurrent.MVar
import Control.Lens
import GHC.Exts

class HasMModel c cm | c -> cm where
    mModel :: Lens' c (MVar cm)

class HasCModel c cm | c -> cm where
    cModel :: Lens' c cm

type family HasSuperModel c cm :: Constraint
type instance HasSuperModel c cm = (HasMModel c cm, HasCModel c cm)
