module Glazier.React.Obj
( WeakObj
, modelWeakRef
, modelWeakVar
, GetWeakObj(..)
, Obj
, modelRef
, modelVar
, benignDeRefWeakObj
, benignReadObj
) where

import Control.Monad.Trans
import Glazier.Benign
import Glazier.React.Model
import Glazier.React.Obj.Internal

benignReadObj :: MonadIO m => Obj s -> Benign m (Model s)
benignReadObj obj = benignReadIORef $ modelRef obj
