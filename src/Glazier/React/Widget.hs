{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Widget where
    -- ( Window
    -- , Widget
    -- , Gadget
    -- , AskWindow(..)
    -- , PutMarkup
    -- , MetaReader
    -- , classNames
    -- , prop
    -- , strProp
    -- , propM
    -- , displayWeakObj
    -- , rawTxt
    -- , lf
    -- , bh
    -- ) where

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Extras
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import qualified Data.DList as DL
import Data.IORef
import qualified Data.JSString as J
import Glazier.Command
import Glazier.React.Markup
import Glazier.React.Model
import Glazier.React.ReactId
import Glazier.React.Shim
import qualified JavaScript.Extras as JE
import System.Mem.Weak

-- type MetaReader s = MaybeT (ReaderT s (Benign IO))
-- type BIOReader s = ReaderT s (Benign IO)

-- | 'Gizmo' is an instance of 'MonadWidget'
-- Gizmo contains the effects (eg register listener)
-- as well as the html for rendering.
-- It is expected that the interpreter of the Gizmo will add a final effect
-- which is to set the final html for the component.
type Widget c s =
    ReaderT (WeakRef Plan) -- 'AskPlanWeakRef', 'AskLogLevel'
    (ReaderT (Model (WeakRef s)) -- 'AskModelWeakRef'
    (ReaderT (Model s) -- 'AskModel'
    (MaybeT -- 'Alternative'
    (ContT () -- 'MonadDelegate'
    -- State monads must be after MonadDelegate
    (StateT (ReactId) -- 'PutReactId'
    (StateT (DL.DList ReactMarkup) -- 'PutMarkup'
    (ProgramT c IO -- 'MonadComand', 'MonadIO'
    )))))))

wack :: Lens' s Bool -> Widget c s Bool
wack lns = do
    s <- askModel
    pure $ s ^.lns


-- | ALlow additional user ReaderT and IdentityT stack on top of Widget c s
-- Like 'Control.Monad.IO.Unlift.UnliftIO', this newtype wrapper prevents impredicative types.
newtype UniftWidget c s m = UniftWidget { unliftWidget :: forall a. m a -> Widget c s a }

-- | Similar to 'Control.Monad.IO.Unlift.MonadUnliftIO', except we want to unlift a @Widget (Gizmo c s) a@.
-- This limits transformers stack to 'ReaderT' and 'IdentityT' on top of @Gizmo c s m@
class MonadUnliftWidget c s m | m -> c s where
    askUnliftWidget :: m (UniftWidget c s m)

instance MonadUnliftWidget c s (Widget c s) where
    askUnliftWidget = pure (UniftWidget id)

instance (Functor m, MonadUnliftWidget c s m) => MonadUnliftWidget c s (ReaderT r m) where
    askUnliftWidget = ReaderT $ \r ->
        (\u -> UniftWidget (unliftWidget u . flip runReaderT r)) <$> askUnliftWidget

instance (Functor m, MonadUnliftWidget c s m) => MonadUnliftWidget c s (IdentityT m) where
    askUnliftWidget = IdentityT $
        (\u -> UniftWidget (unliftWidget u . runIdentityT)) <$> askUnliftWidget

-- newtype Widget m a = Widget (m a)
--     deriving
--         (Monad
--         , Functor
--         , Applicative
--         , Alternative
--         , MonadAsk r
--         , MonadPut s
--         , MonadProgram c
--         , MonadCodify c
--         , MonadBenignIO
--         )

-- wock :: forall s m. (MonadBenignIO m, PutMarkup m, AskPlanWeakRef m) => m ()
-- wock _ = do
--     obj <- askWeakModelRef @s
--     displayWeakModelRef obj

-- wack :: forall c s. Gizmo c s ()
-- wack = wock (Proxy @s)

-- wack2 :: Widget c s ()
-- wack2 = wock

-- FIMXE: Add instances of Gizmo for Widget wrapper

-- | get soemthing from the model as a property
-- prop :: (JE.ToJS a, AskModel s m) => Getting a s a -> m J.JSVal
-- prop lns = (JE.toJSRep . view lns) <$> askModel

-- prop :: (Applicative m, JE.ToJS a) => a -> m J.JSVal
-- prop = pure . JE.toJSRep

-- strProp :: Applicative m => J.JSString -> m J.JSVal
-- strProp = pure . JE.toJSRep

-- -- | Creates a J.JSVal single string for "className" property from a list of (JSString, Bool)
-- -- Idea from https://github.com/JedWatson/classnames
-- classNames :: [(J.JSString, ReaderT s (Benign IO) Bool)] -> ReaderT s (Benign IO) J.JSVal
-- classNames ls = do
--     mdl <- ask
--     -- lift $ lift $ fmap go $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) ls
--     lift $ fmap go $ (`runReaderT` mdl) $ traverse sequenceA ls
--   where go = JE.toJSRep . J.unwords . fmap fst . filter snd -- . catMaybes

-- -- | Creates a J.JSVal single string for "className" property from a list of (JSString, Bool)
-- -- Idea from https://github.com/JedWatson/classnames
-- classNames :: Monad m => [(J.JSString, m Bool)] -> m J.JSVal
-- classNames ls = do
--     mdl <- ask
--     -- lift $ lift $ fmap go $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) ls
--     lift $ fmap go $ (`runReaderT` mdl) $ traverse sequenceA ls
--   where go = JE.toJSRep . J.unwords . fmap fst . filter snd -- . catMaybes

displayPlanWeakRef :: (MonadIO m, PutMarkup m) => WeakRef Plan -> m ()
displayPlanWeakRef that = (`evalMaybeT` ()) $ do
    mdlRef <- MaybeT $ liftIO $ deRefWeak that
    displayPlanRef mdlRef

displayPlanRef :: (MonadIO m, PutMarkup m) => IORef Plan -> m ()
displayPlanRef mdlRef = do
    mdl <- liftIO $ readIORef mdlRef
    displayPlan mdl

displayPlan :: PutMarkup m => Plan -> m ()
displayPlan mdl = do
    -- mdl <- liftBenignIO $ Benign $ readIORef obj
    let scb = mdl ^. _shimCallbacks
        renderCb = shimOnRender scb
        mountedCb = shimOnMounted scb
        renderedCb = shimOnRendered scb
        refCb = shimOnRef scb
        i = mdl ^. _planId
    -- These are the callbacks on the 'ShimComponent'
    -- See jsbits/react.js
    leafMarkup (JE.toJS shimComponent)
        [ ("key", JE.toJS . J.pack $ show i)
        , ("render", JE.toJS renderCb)
        , ("mounted", JE.toJS mountedCb)
        , ("rendered", JE.toJS renderedCb)
        , ("ref", JE.toJS refCb)
        ]

-- | write some text from the model. Use 'like' to 'const' a string instead.
rawTxt :: (PutMarkup m, AskModel s m) => Getting J.JSString s J.JSString -> m ()
rawTxt lns = do
    s <- askModel
    rawTextMarkup $ s ^. lns

-- data Prop = forall a. JE.ToJS a => Prop a

-- -- | Interactive version of 'leafMarkup' using listeners obtained from the 'Plan' for the local 'ReactId'.
-- -- It return a 'Widget' to indicate that it contains a 'Window' or possibly modifies 'PutReactId'.
-- -- 'Gadget' are guaranteed to be created from 'Glazier.React.Reactor.trigger' or the like.
-- -- 'Gadget' must not contain any 'Widget', otherwise this function will result in nested
-- -- @Widget (Widget m) a@ which indicates at compile time incorrect usage,
-- lf ::
--     ( PutMarkup s m
--     , PutReactId m
--     , Also () m
--     , MonadDelegate m
--     )
--     => J.JSString-- ^ eg "div" or "input"
--     -> DL.DList (Gadget m ())
--     -> DL.DList (J.JSString, MetaReader s J.JSVal)
--     -> Widget m ()
-- lf n gads props = Widget $ do
--     -- make sure the react id is unique amongst siblings
--     modifyReactId $ \(ReactId (_ NE.:| ns, i)) -> ReactId (n NE.:| ns, i + 1)
--     i <- askReactId
--     -- run through the initialization of all the gadgets which will use this key
--     -- 'finish' each gadget to ensure the event handling doens't leak out
--     -- and combine using 'also' with @pure ()@ so execution will get to the next line.
--     foldr' (also . finish . runGadget) (pure ()) gads
--     runWidget $ appendWindow $ do
--         ls <- getReactListeners i -- get the listeners created by gads above
--         mdl <- view _meta
--         props' <- lift $ fmap catMaybes $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) (DL.toList props)
--         leafMarkup (JE.toJSRep n) (("key", JE.toJSRep . J.pack $ show i) `DL.cons` DL.fromList props' <> DL.fromList ls)

-- -- | Interactive version of 'branchMarkup' using listeners obtained from the 'Plan' for the local 'ReactId'.
-- -- It return a 'Widget' to indicate that it contains a 'Window' or possibly modifies 'PutReactId'.
-- -- 'Gadget' are guaranteed to be created from 'Glazier.React.Reactor.trigger' or the like.
-- -- 'Gadget' must not contain any 'Widget', otherwise this function will result in nested
-- -- @Widget (Widget m) a@ which indicates at compile time incorrect usage,
-- bh ::
--     ( PutMarkup s m
--     , PutReactId m
--     , Also () m
--     , MonadDelegate m
--     )
--     => J.JSString-- ^ eg "div" or "input"
--     -> DL.DList (Gadget m ())
--     -> DL.DList (J.JSString, MetaReader s J.JSVal)
--     -- Gadget are guaranteed not to 'appendWindow' or 'modifyReactId',
--     -- and only contains a single trigger each.
--     -> Widget m a
--     -> Widget m a
-- bh n gads props (Widget child) = Widget $ do
--     -- make sure the react id is unique amongst siblings
--     modifyReactId $ \(ReactId (_ NE.:| ns, i)) -> ReactId (n NE.:| ns, i + 1)
--     i <- askReactId
--     -- run through the initialization of all the gadgets which will use this key
--     -- 'finish' each gadget to ensure the event handling doens't leak out
--     -- and combine using 'also' with @pure ()@ so execution will get to the next line.
--     foldr' (also . finish . runGadget) (pure ()) gads
--     delegate $ \fire -> do
--         -- 'also' with @pure ()@ to protect against 'finish'
--         childWin <- runWidget $ bracketWindow $ runWidget $ bracketReactId $ (child >>= fire) `also` (pure ())
--         -- now we can add the branch node with the children's window
--         runWidget $ appendWindow $ do
--             ls <- getReactListeners i -- get the listeners created by gads above
--             mdl <- view _meta
--             props' <- lift $ fmap catMaybes $ (`runReaderT` mdl) $ traverse (runMaybeT . sequenceA) (DL.toList props)
--             branchMarkup (JE.toJSRep n) (("key", JE.toJSRep . J.pack $ show i) `DL.cons` DL.fromList props' <> DL.fromList ls) childWin




-- wack :: (MonadReader Int m) => n a -> m a
-- wack = coerce


-- wock :: MonadReader Int n => n Int
-- wock = ask

-- wock2 :: MonadReader Int m => m Int
-- wock2 = wack wock

-- keyProperty :: ReactId -> JE.Property
-- keyProperty k = ("key", JE.toJSRep . J.pack $ show k)

-- bindListenerContext :: J.JSVal -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> J.JSVal
-- bindListenerContext = js_bindListenerContext

-- #ifdef __GHCJS__

-- foreign import javascript unsafe
--     "$r = function(j) { $2($1, j) };"
--     js_bindListenerContext :: J.JSVal -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> J.JSVal

-- #else

-- js_bindListenerContext :: J.JSVal -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> J.JSVal
-- js_bindListenerContext _ _ = J.JSVal J.nullRef


-- #endif




-- -- | Create a MonadState that run the given given a combining function
-- -- where the first arg is the state from running the markup producing MonadState with mempty,
-- -- and the 2nd arg the starting state of the resultant MonadState.
-- withWindow :: PutMarkup s m
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
-- import Glazier.React.Meta
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
-- -- magnifyWidget l wid = ExceptT $ (first (magnifiedMeta l)) <$> (magnifiedEntity l (runExceptT wid))

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
