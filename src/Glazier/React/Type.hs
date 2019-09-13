{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Type where

import Data.IORef
import Glazier.ShowIO
import Glazier.Logger
import System.Mem.Weak
import qualified GHCJS.Types as J

type Ref s = IORef s
type WeakRef s = Weak (IORef s)

type ShowIOJS = ShowIO J.JSString

type LoggerJS c m = Logger J.JSString c m
type LogNameJS = LogName J.JSString