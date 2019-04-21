{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Obj
( WeakObj
-- , HasWeakObj(..)
, GetWeakObj(..)
, sceneWeakRef
, sceneWeakVar
-- , self
, Obj
, ToObj(..)
-- , HasObj(..)
, sceneRef
, sceneVar
, weakObj
-- , benignDeRefWeakObj
, benignReadObjScene
) where

-- import Data.Diverse.Lens
import Control.Lens
import Control.Monad.Trans.Maybe
import Glazier.Benign
import Glazier.React.Obj.Internal
import Glazier.React.Scene

-- | Get a 'Lens' to a 'WeakObj'.
-- The 'Setter' in the 'Lens' is useful to be used in a 'MonadReader' environment
-- so that it is easy to run a 'MonadRader' for a different 'WeakObj'
-- without knowing anything else about the environment.
-- The @r -> o@ functional dependency resolves ambiguity with @o@
-- class (Has (WeakObj s) r) => HasWeakObj s r | r -> s where
--     _weakObj :: Lens' r (WeakObj s)
--     _weakObj = hasLens @(WeakObj s)

-- instance HasWeakObj s (WeakObj s) where
--     _weakObj = id

-- -- | A 'WeakObj' is ananolgou to the @self@ pointer in python.
-- self :: (MonadReader r m, HasWeakObj s r) => m (WeakObj s)
-- self = view _weakObj

-- | A restricted form of 'HasWeakObj' limiting the lens to a 'Getter'.
-- 'Obj' only has an instance of 'GetWeakObj'
-- The @r -> o@ functional dependency resolves ambiguity with @o@
class GetWeakObj s r | r -> s where
    _getWeakObj :: Getter r (WeakObj s)

-- instance {-# OVERLAPPABLE #-} HasWeakObj s r => GetWeakObj s r where
--     _getWeakObj = _weakObj

instance GetWeakObj s (WeakObj s) where
    _getWeakObj = id

-- _weakObj :: forall s t. Has (WeakObj s) t => Lens' t (WeakObj s)
-- _weakObj = hasLens @(WeakObj s)

-- | The @r -> o@ functional dependency resolves ambiguity with @o@
-- class (Has (Obj s) r) => HasObj s r | r -> s where
--     _obj :: Lens' r (Obj s)
--     _obj = hasLens @(Obj s)

-- instance HasObj s (Obj s) where
--     _obj = id

instance GetWeakObj s (Obj s) where
    _getWeakObj = to weakObj

class ToObj s r | r -> s where
    toObj :: MonadBenignIO m => r -> m (Maybe (Obj s))

instance ToObj s (Obj s) where
    toObj obj = pure (Just obj)

instance ToObj s (WeakObj s) where
    toObj wkObj = runMaybeT $ Obj wkObj <$> scnRef <*> scnVar
      where
        scnWkRef = sceneWeakRef wkObj
        scnWkVar = sceneWeakVar wkObj
        scnRef = MaybeT . liftBenignIO . benignDeRefWeak $ scnWkRef
        scnVar = MaybeT . liftBenignIO . benignDeRefWeak $ scnWkVar

-- _obj :: forall s t. Has (Obj s) t => Lens' t (Obj s)
-- _obj = hasLens @(Obj s)

benignReadObjScene :: MonadBenignIO m => Obj s -> m (Scene s)
benignReadObjScene obj = liftBenignIO $ benignReadIORef $ sceneRef obj
