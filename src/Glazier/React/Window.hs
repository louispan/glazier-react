{-# LANGUAGE CPP #-}
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
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Window where

import Control.Also
import Control.Applicative
import Control.Lens
import Control.Monad.Benign
import Control.Monad.Delegate
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import Glazier.React.Markup
import Glazier.React.Obj
import Glazier.React.ReactId
import Glazier.React.Scene
import Glazier.React.Shim
import qualified JavaScript.Extras as JE

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- The @s@ can be magnified with 'magnifiedScene'
-- 'Window' is an instance of 'MonadBenignIO' and 'MonadState (DL.DList ReactMarkup)'
type Window s = RWST (Scene s) () (DL.DList ReactMarkup) (Benign IO)

getListeners :: MonadReader (Scene s) m => ReactId -> m [(J.JSString, JE.JSRep)]
getListeners k = do
    ls <- view (_plan._reactants.ix k._reactListeners.to M.toList)
    pure $ (\(n, (cb, _)) -> (n, JE.toJSRep cb)) <$> ls

displayWeakObj :: (MonadBenignIO m, MonadState (DL.DList ReactMarkup) m) => WeakObj o -> m ()
displayWeakObj obj = (`evalMaybeT` ()) $ do
    scn <- MaybeT $ benignReadWeakObjScene obj
    let scb = scn ^. _plan._shimCallbacks
        renderCb = shimOnRender scb
        mountedCb = shimOnMounted scb
        renderedCb = shimOnRendered scb
        refCb = shimOnRef scb
        k = scn ^. _plan._planId
    -- These are the callbacks on the 'ShimComponent'
    -- See jsbits/react.js
    leaf (JE.toJSRep shimComponent)
        [ ("key", JE.toJSRep . J.pack $ show k)
        , ("render", JE.toJSRep renderCb)
        , ("mounted", JE.toJSRep mountedCb)
        , ("rendered", JE.toJSRep renderedCb)
        , ("ref", JE.toJSRep refCb)
        ]

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

modifyWindow :: PutWindow s m => (Window s () -> Window s ()) -> m ()
modifyWindow f = do
  s <- askWindow
  putWindow $! f s

appendWindow :: (PutWindow s m) => Window s () -> m ()
appendWindow a = modifyWindow (*> a)

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

rawTxt :: PutWindow s m => J.JSString -> m ()
rawTxt n = appendWindow $ rawText n

lf ::
    ( PutWindow s m
    , PutReactId m
    )
    => J.JSString
    -> DL.DList (J.JSString, Prop s)
    -> m ()
lf n props = do
    -- make sure the react id is unique amongst siblings
    modifyReactId $ \(ReactId (_ NE.:| ns, i)) -> ReactId (n NE.:| ns, i + 1)
    k <- askReactId
    appendWindow $ do
        mdl <- view _model
        props' <- lift $ fmap catMaybes $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) (DL.toList props)
        leaf (JE.toJSRep n) (("key", JE.toJSRep . J.pack $ show k) `DL.cons` (DL.fromList props'))

-- | Interactive version of 'lf' using listeners obtained from the 'Plan' for the local 'ReactId'.
lf' ::
    ( PutWindow s m
    , PutReactId m
    , Also () m
    )
    => J.JSString-- ^ eg "div" or "input"
    -> DL.DList (J.JSString, Prop s)
    -> DL.DList (m ())
    -> m ()
lf' n props gads = do
    -- make sure the react id is unique amongst siblings
    modifyReactId $ \(ReactId (_ NE.:| ns, i)) -> ReactId (n NE.:| ns, i + 1)
    k <- askReactId
    -- run through the initialization of all the gadgets which will use this key
    -- combine using 'also' with @pure ()@ to protect again 'finish'
    foldr' also (pure ()) gads
    appendWindow $ do
        mdl <- view _model
        props' <- lift $ fmap catMaybes $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) (DL.toList props)
        ls <- getListeners k -- get the listeners created by gads above
        leaf (JE.toJSRep n) (("key", JE.toJSRep . J.pack $ show k) `DL.cons` DL.fromList props' <> DL.fromList ls)

bh :: ( PutWindow s m
    , PutReactId m
    , Also () m
    , MonadDelegate m
    )
    => J.JSString
    -> DL.DList (J.JSString, Prop s)
    -> m a
    -> m a
bh n props m = bh' n props [] m

-- | Interactive version of 'bh' using listeners obtained from the 'Plan' for the local 'ReactId'.
bh' ::
    ( PutWindow s m
    , PutReactId m
    , Also () m
    , MonadDelegate m
    )
    => J.JSString-- ^ eg "div" or "input"
    -> DL.DList (J.JSString, Prop s)
    -> DL.DList (m ())
    -> m a
    -> m a
bh' n props gads childs = do
    -- make sure the react id is unique amongst siblings
    modifyReactId $ \(ReactId (_ NE.:| ns, i)) -> ReactId (n NE.:| ns, i + 1)
    k <- askReactId
    -- run through the initialization of all the gadgets which will use this key
    -- combine using 'also' with @pure ()@ to protect again 'finish'
    foldr' also (pure ()) gads
    delegate $ \fire -> do
        -- save current window
        s <- askWindow
        -- prepare to run children with blank window
        putWindow (pure ())
        -- prepare to run the children with a locally scoped modified reactid, pushing this name in the list of names
        modifyReactId $ \(ReactId (ns, _)) -> ReactId (mempty NE.<| ns, 0)
        -- 'also' with @pure ()@ to protect against 'finish'
        (childs >>= fire) `also` (pure ())
        -- get the children's window
        childWin <- askWindow
        -- restore original window
        putWindow s
        -- restore this key
        putReactId k
        -- now we can add the branch node with the children's window
        appendWindow $ do
            mdl <- view _model
            props' <- lift $ fmap catMaybes $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) (DL.toList props)
            ls <- getListeners k -- get the listeners created by gads above
            branch (JE.toJSRep n) (("key", JE.toJSRep . J.pack $ show k) `DL.cons` DL.fromList props' <> DL.fromList ls) childWin

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
