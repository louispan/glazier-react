{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Shared where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens

-- | Record accessor for a MVar
class HasMVar c a | c -> a where
    mvar :: Lens' c (MVar a)

instance HasMVar (MVar a) a where
    mvar = id
    {-# INLINE mvar #-}

-- | Record accessor for a immutable value (as opposed to 'HasMVar')
class HasIVal c a | c -> a where
    ival :: Lens' c a


----------------------------------------------------------
-- | Something that has an immutable component, as well as a MVar that
-- can be used to share a value with other threads.
-- This is used by the gadget to be able to purely manipulate a value
-- as well as put it into an MVar for other threads to access the value.
data Shared a = Shared (MVar a) a

instance CD.Disposing a => CD.Disposing (Shared a) where
    disposing (Shared _ a) = CD.disposing a
    {-# INLINE disposing #-}

instance HasMVar (Shared mdl) mdl where
    mvar f (Shared mv iv) = fmap (\mv' -> Shared mv' iv) (f mv)
    {-# INLINE mvar #-}

instance HasIVal (Shared mdl) mdl where
    ival f (Shared mv iv) = fmap (\iv' -> Shared mv iv') (f iv)
    {-# INLINE ival #-}
