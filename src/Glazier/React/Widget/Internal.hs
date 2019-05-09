{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Widget.Internal where

import Control.Also
import Control.Lens
import Control.Monad.Benign
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import qualified Data.DList as DL
import Data.Functor.Functor2
import qualified Data.JSString as J
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import Glazier.React.Markup
import Glazier.React.ReactId.Internal
import Glazier.React.Scene
import qualified JavaScript.Extras as JE

-- | A wrapper to indicate that the inner monad was made from one of the
-- 'Glazier.React.Reactor.trigger' functions.
-- The 'Gadget' wrapper is used to ensure that it is only within the gadgets arg of
-- 'Glazier.Window.lf' or 'Glazier.Window.bh'.
-- Not an instance of 'Applicative' or 'Monad' as 'Gadget' cannot be combined
-- with other Gadgets except with 'Also'.
newtype Gadget m a = Gadget { runGadget :: m a }
    deriving (G.Generic, G.Generic1, Functor)

instance Functor2 Gadget where
    fmap2 f (Gadget m) = Gadget $ f m

instance Also r m => Also r (Gadget m) where
    alsoZero = Gadget alsoZero
    (Gadget m) `also` (Gadget n) = Gadget (m `also` n)

instance Also a m => Semigroup (Gadget m a) where
    (<>) = also

instance Also a m => Monoid (Gadget m a) where
    mempty = alsoZero

-- | A wrapper to indicate that the inner monad was made from
-- 'Glazier.React.Window.lf' or 'Glazier.React.Window.bh' functions.
-- The 'Widget' wrapper is used to indicate that it contains a 'Window' and
-- that 'PutReactId' was used.
-- Not an instance of 'Applicative' or 'Monad' as 'Widget' cannot be combined
-- with other Widget except with 'Also'.
newtype Widget m a = Widget { runWidget :: m a }
    deriving (G.Generic, G.Generic1, Functor)

instance Functor2 Widget where
    fmap2 f (Widget m) = Widget $ f m

instance Also r m => Also r (Widget m) where
    alsoZero = Widget alsoZero
    (Widget m) `also` (Widget n) = Widget (m `also` n)

instance Also a m => Semigroup (Widget m a) where
    (<>) = also

instance Also a m => Monoid (Widget m a) where
    mempty = alsoZero

-----------------------------------------------
-- The @s@ can be magnified with 'magnifiedScene'
-- 'Window' is an instance of 'MonadBenignIO' and 'MonadState (DL.DList ReactMarkup)'
type Window s = RWST (Scene s) () (DL.DList ReactMarkup) (Benign IO)

-- | A copy of 'MonadReader' with overlapping instances
class Monad m => AskWindow s m | m -> s where
    askWindow :: m (Window s ())

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, AskWindow s m) => AskWindow s (t m) where
    askWindow = lift askWindow

instance {-# OVERLAPPABLE #-} Monad m => AskWindow s (ReaderT (Window s ()) m) where
    askWindow = ask

instance {-# OVERLAPPABLE #-} Monad m => AskWindow s (StateT (Window s ()) m) where
    askWindow = get

class AskWindow s m => PutWindow s m | m -> s where
    putWindow :: Window s () -> m ()

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, PutWindow s m) => PutWindow s (t m) where
    putWindow = lift . putWindow

instance {-# OVERLAPPABLE #-} Monad m => PutWindow s (StateT (Window s ()) m) where
    putWindow = put

modifyWindow :: PutWindow s m => (Window s () -> Window s ()) -> Widget m ()
modifyWindow f = Widget $ do
  s <- askWindow
  putWindow $! f s

appendWindow :: PutWindow s m => Window s () -> Widget m ()
appendWindow a = modifyWindow (*> a)

bracketReactId :: PutReactId m => m () -> Widget m ()
bracketReactId m = Widget $ do
    i <- askReactId
    -- prepare to run the children with a locally scoped modified reactid, pushing this name in the list of names
    modifyReactId $ \(ReactId (ns, _)) -> ReactId (mempty NE.<| ns, 0)
    m
    -- restore the original i
    putReactId i

bracketWindow :: PutWindow s m => m () -> Widget m (Window s ())
bracketWindow child = Widget $ do
    -- save current window
    s <- askWindow
    -- prepare to run children with blank window
    putWindow (pure ())
    -- 'also' with @pure ()@ to protect against 'finish'
    child
    -- get the children's window
    childWin <- askWindow
    -- restore original window
    putWindow s
    pure childWin

getReactListeners :: MonadReader (Scene s) m => ReactId -> m [(J.JSString, JE.JSRep)]
getReactListeners i = do
    ls <- view (_plan._reactants.ix i._reactListeners.to M.toList)
    pure $ (\(n, (cb, _)) -> (n, JE.toJSRep cb)) <$> ls
