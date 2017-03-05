{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Model.Class where

import Control.Concurrent.MVar
import Control.Lens

class HasMModel c cm | c -> cm where
    mModel :: Lens' c (MVar cm)

class HasCModel c cm | c -> cm where
    cModel :: Lens' c cm

class (HasMModel c cm, HasCModel c cm) => HasSuperModel c cm | c -> cm
