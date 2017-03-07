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

class HasMModel c gm | c -> gm where
    mModel :: Lens' c (MVar gm)

class HasGModel c gm | c -> gm where
    gModel :: Lens' c gm

type family HasSuperModel c gm :: Constraint
type instance HasSuperModel c gm = (HasMModel c gm, HasGModel c gm)
