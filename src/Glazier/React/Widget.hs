{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Widget
    ( Window
    , Widget
    , Gadget
    , AskWindow(..)
    , PutWindow(..)
    , Prop
    , ToProp(..)
    , strProp
    , propM
    , displayWeakObj
    , rawTxt
    , lf
    , bh
    ) where

import Control.Also
import Control.Applicative
import Control.Lens
import Control.Monad.Benign
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Glazier.React.ReactId.Internal
import Glazier.React.Widget.Internal
import Glazier.React.Markup
import Glazier.React.Obj
import Glazier.React.Scene
import Glazier.React.Shim
import qualified JavaScript.Extras as JE

type Prop s = MaybeT (ReaderT s (Benign IO)) JE.JSRep

-- | A convenience class to make using 'lf' and 'bh' properties easier.
-- It converts monad that result into @a@ or @Maybe a@ into 'JSRep'
class ToProp a m where
    prop :: a -> m JE.JSRep

instance {-# OVERLAPPABLE #-} (Applicative m, JE.ToJS a) => ToProp a m where
    prop = pure . JE.toJSRep

-- | reduce 'Maybe' using the 'Alternative' instance.
instance {-# OVERLAPPABLE #-} (Monad m, Alternative m, JE.ToJS a) => ToProp (Maybe a) m where
    prop = fmap JE.toJSRep . whenJust

-- | Handy when using overloaded lists
strProp :: Applicative m => J.JSString -> m JE.JSRep
strProp = prop

propM :: (Monad m, ToProp a m) => m a -> m JE.JSRep
propM = (>>= prop)

displayWeakObj :: (MonadBenignIO m, MonadState (DL.DList ReactMarkup) m) => WeakObj o -> m ()
displayWeakObj obj = (`evalMaybeT` ()) $ do
    scn <- MaybeT $ benignReadWeakObjScene obj
    let scb = scn ^. _plan._shimCallbacks
        renderCb = shimOnRender scb
        mountedCb = shimOnMounted scb
        renderedCb = shimOnRendered scb
        refCb = shimOnRef scb
        i = scn ^. _plan._planId
    -- These are the callbacks on the 'ShimComponent'
    -- See jsbits/react.js
    leafMarkup (JE.toJSRep shimComponent)
        [ ("key", JE.toJSRep . J.pack $ show i)
        , ("render", JE.toJSRep renderCb)
        , ("mounted", JE.toJSRep mountedCb)
        , ("rendered", JE.toJSRep renderedCb)
        , ("ref", JE.toJSRep refCb)
        ]

rawTxt :: PutWindow s m => ReaderT s (Benign IO) J.JSString -> Widget m ()
rawTxt m = appendWindow $ do
    s <- view _model
    n <- lift $ runReaderT m s
    rawTextMarkup n

-- | Interactive version of 'leafMarkup' using listeners obtained from the 'Plan' for the local 'ReactId'.
-- It return a 'Widget' to indicate that it contains a 'Window' or possibly modifies 'PutReactId'.
-- 'Gadget' are guaranteed to be created from 'Glazier.React.Reactor.trigger' or the like.
-- 'Gadget' must not contain any 'Widget', otherwise this function will result in nested
-- @Widget (Widget m) a@ which indicates at compile time incorrect usage,
lf ::
    ( PutWindow s m
    , PutReactId m
    , Also () m
    , MonadDelegate m
    )
    => J.JSString-- ^ eg "div" or "input"
    -> DL.DList (J.JSString, Prop s)
    -> DL.DList (Gadget m ())
    -> Widget m ()
lf n props gads = Widget $ do
    -- make sure the react id is unique amongst siblings
    modifyReactId $ \(ReactId (_ NE.:| ns, i)) -> ReactId (n NE.:| ns, i + 1)
    i <- askReactId
    -- run through the initialization of all the gadgets which will use this key
    -- 'finish' each gadget to ensure the event handling doens't leak out
    -- and combine using 'also' with @pure ()@ so execution will get to the next line.
    foldr' (also . finish . runGadget) (pure ()) gads
    runWidget $ appendWindow $ do
        ls <- getReactListeners i -- get the listeners created by gads above
        mdl <- view _model
        props' <- lift $ fmap catMaybes $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) (DL.toList props)
        leafMarkup (JE.toJSRep n) (("key", JE.toJSRep . J.pack $ show i) `DL.cons` DL.fromList props' <> DL.fromList ls)

-- | Interactive version of 'branchMarkup' using listeners obtained from the 'Plan' for the local 'ReactId'.
-- It return a 'Widget' to indicate that it contains a 'Window' or possibly modifies 'PutReactId'.
-- 'Gadget' are guaranteed to be created from 'Glazier.React.Reactor.trigger' or the like.
-- 'Gadget' must not contain any 'Widget', otherwise this function will result in nested
-- @Widget (Widget m) a@ which indicates at compile time incorrect usage,
bh ::
    ( PutWindow s m
    , PutReactId m
    , Also () m
    , MonadDelegate m
    )
    => J.JSString-- ^ eg "div" or "input"
    -> DL.DList (J.JSString, Prop s)
    -- Gadget are guaranteed not to 'appendWindow' or 'modifyReactId',
    -- and only contains a single trigger each.
    -> DL.DList (Gadget m ())
    -> Widget m a
    -> Widget m a
bh n props gads (Widget child) = Widget $ do
    -- make sure the react id is unique amongst siblings
    modifyReactId $ \(ReactId (_ NE.:| ns, i)) -> ReactId (n NE.:| ns, i + 1)
    i <- askReactId
    -- run through the initialization of all the gadgets which will use this key
    -- 'finish' each gadget to ensure the event handling doens't leak out
    -- and combine using 'also' with @pure ()@ so execution will get to the next line.
    foldr' (also . finish . runGadget) (pure ()) gads
    delegate $ \fire -> do
        -- 'also' with @pure ()@ to protect against 'finish'
        childWin <- runWidget $ bracketWindow $ runWidget $ bracketReactId $ (child >>= fire) `also` (pure ())
        -- now we can add the branch node with the children's window
        runWidget $ appendWindow $ do
            ls <- getReactListeners i -- get the listeners created by gads above
            mdl <- view _model
            props' <- lift $ fmap catMaybes $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) (DL.toList props)
            branchMarkup (JE.toJSRep n) (("key", JE.toJSRep . J.pack $ show i) `DL.cons` DL.fromList props' <> DL.fromList ls) childWin

-- wack :: (MonadReader Int m) => n a -> m a
-- wack = coerce


-- wock :: MonadReader Int n => n Int
-- wock = ask

-- wock2 :: MonadReader Int m => m Int
-- wock2 = wack wock

-- keyProperty :: ReactId -> JE.Property
-- keyProperty k = ("key", JE.toJSRep . J.pack $ show k)

-- bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
-- bindListenerContext = js_bindListenerContext

-- #ifdef __GHCJS__

-- foreign import javascript unsafe
--     "$r = function(j) { $2($1, j) };"
--     js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep

-- #else

-- js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
-- js_bindListenerContext _ _ = JE.JSRep J.nullRef


-- #endif




-- -- | Create a MonadState that run the given given a combining function
-- -- where the first arg is the state from running the markup producing MonadState with mempty,
-- -- and the 2nd arg the starting state of the resultant MonadState.
-- withWindow :: PutWindow s m
--     => (Window s () -> Window s () -> Window s ())
--     -> m a
--     -> m a
-- withWindow f childs = do
--     -- save state
--     s <- askWindow
--     -- run children with mempty
--     putWindow mempty
--     a <- childs
--     childs' <- askWindow
--     -- restore state
--     putWindow s
--     modifyWindow (f childs')
--     pure a

-- import Control.Lens
-- import Control.Monad.Except
-- import Data.Bifunctor
-- import Data.Diverse.Lens
-- import Data.Diverse.Profunctor
-- import Glazier.Command.Exec
-- import Glazier.React.Entity
-- import Glazier.React.Gadget
-- import Glazier.React.Model
-- import Glazier.React.Window

-- -- | A 'Widget' is a gadget that fires 'Either' a 'Window' or an event.
-- type MonadWidget c s m= (MonadGadget c s m, MonadError (Window s ()) m)

-- type Widget c s = ExceptT (Window s ()) (Gadget c)

-- -- -- | Pass the same MonadWidget into this function to verify at compile time
-- -- -- that a concrete instance of widget doesn't require any @AsFacet (IO c) c@.
-- -- -- LOUISFIXME: is there a simpler way @c ~ NoIOCmd c@?
-- -- noIOWidget :: Widget (NoIOCmd c) s s a -> Widget c s s a -> Widget c s s a
-- -- noIOWidget _ = id

-- -- magnifyWidget :: Traversal' t s -> ExceptT (Window s ()) (Gadget c s s') a -> ExceptT (Window t ()) (Gadget c o t) a
-- -- magnifyWidget l wid = ExceptT $ (first (magnifiedModel l)) <$> (magnifiedEntity l (runExceptT wid))

-- -- | Convert a 'Gadget' into a 'Widget'
-- widget :: Gadget c s s' (Either (Window s ()) a) -> Widget c s s' a
-- widget = ExceptT

-- runWidget :: Widget c s s' a -> Gadget c s s' (Either (Window s ()) a)
-- runWidget = runExceptT

-- -- mapWidget ::
-- --     (Gadget c s s' (Either (Window s ()) a) -> Gadget c o' s' (Either (Window s' ()) b))
-- --     -> Widget c s s' a -> Widget c o' s' b
-- -- mapWidget = mapExceptT

-- display :: Window s () -> Widget c s s' a
-- display = throwError

-- display' :: Window s () -> Widget c s s' (Which '[])
-- display' = throwError

-- overWindow :: (Window s () -> Window s ()) -> Widget c s s' a -> Widget c s s' a
-- overWindow = withExceptT

-- overWindow2 :: (Window s () -> Window s () -> Window s ())
--     -> Widget c s s' a -> Widget c s s' a -> Widget c s s' a
-- overWindow2 f x y = withWindow x $ \x' -> withWindow y $ \y' -> display $ f x' y'

-- overWindow3 :: (Window s () -> Window s () -> Window s () -> Window s ())
--     -> Widget c s s' a -> Widget c s s' a -> Widget c s s' a -> Widget c s s' a
-- overWindow3 f x y z = withWindow x $
--     \x' -> withWindow y $
--     \y' -> withWindow z $
--     \z' -> display $ f x' y' z'

-- overWindow2' ::
--     ( ChooseBoth x1 x2 ys)
--     => (Window s () -> Window s () -> Window s ())
--     -> Widget c s s' (Which x1) -> Widget c s s' (Which x2) -> Widget c s s' (Which ys)
-- overWindow2' f x1 x2 = overWindow2 f (diversify <$> x1) (diversify <$> x2)

-- overWindow3' ::
--     ( Diversify x1 ys
--     , Diversify x2 ys
--     , Diversify x3 ys
--     , ys ~ AppendUnique x1 (AppendUnique x2 x3))
--     => (Window s () -> Window s () -> Window s () -> Window s ())
--     -> Widget c s s' (Which x1) -> Widget c s s' (Which x2) -> Widget c s s' (Which x3) -> Widget c s s' (Which ys)
-- overWindow3' f x1 x2 x3 = overWindow3 f
--     (diversify <$> x1)
--     (diversify <$> x2)
--     (diversify <$> x3)

-- withWindow :: Widget c s s' a -> (Window s () -> Widget c s s' a) -> Widget c s s' a
-- withWindow = catchError

-- withWindow' :: (ChooseBoth xs ys zs)
--     => Widget c s s' (Which xs)
--     -> (Window s () -> Widget c s s' (Which ys))
--     -> Widget c s s' (Which zs)
-- withWindow' m f = withWindow (diversify <$> m) (fmap diversify . f)
