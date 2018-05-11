{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Scene where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import Control.Monad.RWS
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tagged
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React.Component
import Glazier.React.EventTarget
import Glazier.React.MkId
import qualified JavaScript.Extras as JE

----------------------------------------------------------------------------------

_once :: Lens' (Tagged "Once" a, Tagged "Always" a) a
_once = (_1._Tagged' @"Once")

_always :: Lens' (Tagged "Once" a, Tagged "Always" a) a
_always = (_2._Tagged' @"Always")

-- | Interactivity for a particular DOM element.
-- type Listener = (J.JSString, J.Callback (J.JSVal -> IO ()))
data Elemental = Elemental
    { elementalRef :: Maybe EventTarget
    -- (name of event, context of event)
    , listeners :: M.Map J.JSString (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Always" (JE.JSRep -> IO ()))
    } deriving (G.Generic)

makeLenses_ ''Elemental

newElemental :: Elemental
newElemental = Elemental Nothing mempty

----------------------------------------------------------------------------------

data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { shimRender :: J.Callback (IO J.JSVal)
    -- Run the doOnRendered in the plan
    , shimRendered :: J.Callback (IO ())
    -- updates the componenRef
    , shimRef :: J.Callback (J.JSVal -> IO ())
    -- all listeners use the same entry function, just a different
    -- first arg context.
    , shimListen :: J.Callback (J.JSVal -> J.JSVal -> IO ())
    } deriving (G.Generic)

makeLenses_ ''ShimCallbacks

instance CD.Dispose ShimCallbacks where
    dispose (ShimCallbacks a b c d) = CD.dispose a <> CD.dispose b <> CD.dispose c <> CD.dispose d

----------------------------------------------------------------------------------

-- | Interactivity data for a react component
data Plan = Plan
    -- Plan rquires planId for 'Glazier.React.Framework.Reactor.MkOnceOnUpdatedCallback'
    -- and 'Glazier.React.Framework.Reactor.MkAlwaysOnUpdatedCallback'
    -- { planId :: PlanId
    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    { componentRef :: Maybe ComponentRef
    , shimCallbacks :: ShimCallbacks
    -- This is the previous "react state"
    -- , previousFrameNum :: Int
    -- -- This the current "react state".
    -- -- The glazier framework engine will use this to send a render request
    -- -- if the current didn't match previous.
    -- , currentFrameNum :: Int
    --   -- Things to dispose when this widget is removed
    --   -- cannot be hidden inside afterOnUpdated, as this also needs to be used
    --   -- when finalizing
    -- -- , disposeOnRemoved :: CD.Disposable
    , doOnRendered :: (Tagged "Once" (IO ()), Tagged "Always" (IO ()))
    --  Things to dispose on updated
    -- , disposeOnUpdated :: CD.Disposable
    -- interactivity data for child DOM elements
    , elementals :: M.Map ElementalId Elemental
    -- interactivity data for child react components
    -- , plans :: M.Map PlanId (MVar Plan)
    } deriving (G.Generic)

makeLenses_ ''Plan

-- instance CD.Dispose Plan where
--     dispose pln = (CD.dispose (shimCallbacks pln))
--         <> (disposeOnUpdated pln)
--         -- <> (foldMap CD.dispose (plans pln))

instance Show Plan where
    showsPrec d pln = showParen
        (d >= 11)
        ( showString "Plan {" . showString "componentRef ? " . shows (isJust $ componentRef pln)
        -- . showString ", " . showString "previousFrameNum = " . shows (previousFrameNum pln)
        -- . showString ", " . showString "currentFrameNum = " . shows (currentFrameNum pln)
        . showString ", " . showString "elementalIds = " . showList (M.keys $ elementals pln)
        . showString ", " . showString "planIds = " . showList (M.keys $ elementals pln)
        . showString "}"
        )

-- newPlan :: Plan
-- newPlan = Plan
--     Nothing
--     Nothing
--     0
--     0
--     (Tagged mempty, Tagged mempty)
--     mempty
--     mempty
--     -- mempty

----------------------------------------------------------------------------------


-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
data Scene s = Scene
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { plan :: Plan
    , model :: s
    } deriving (G.Generic, Show, Functor)

-- class HasModel c c' s s' | c -> s, c' -> s' where
--     _model :: Lens c c' s s'

-- type HasModel' c s = HasModel c c s s

-- _model' :: HasModel' c s => Lens' c s
-- _model' = _model

-- instance HasModel (Scene s) (Scene s') s s' where
--     _model = lens model (\s a -> s { model = a})

_model :: Lens (Scene s) (Scene s') s s'
_model = lens model (\s a -> s { model = a})

_plan :: Lens' (Scene s) Plan
_plan = lens plan (\s a -> s { plan = a})

-- class HasScene c c' s s' | c -> s, c' -> s' where
--     _scene :: Lens c c' (Scene s) (Scene s')

-- type HasScene' c s = HasScene c c s s

-- _scene' :: HasScene' c s => Lens' c (Scene s)
-- _scene' = _scene

-- instance HasScene (Scene s) (Scene s') s s' where
--     _scene = id

editSceneModel :: (Functor f) => LensLike' f s a -> LensLike' f (Scene s) (Scene a)
editSceneModel l safa s = (\s' -> s & _model .~ s' ) <$> l afa' (s ^. _model)
  where
    afa' a = (view _model) <$> safa (s & _model .~ a)

----------------------------------------------------------------------------------

-- -- | A 'Scene' contains interactivity data for all widgets as well as the model data.
-- data Scenario cmd s = Scenario
--     -- commands could be in a writer monad, but then you can't get
--     -- a MonadWriter with ContT, but you can have a MonadState with ContT.
--     { commands :: DL.DList cmd
--     , scene :: Scene s
--     } deriving (G.Generic, Functor)

-- instance HasCommands (Scenario cmd s) (Scenario cmd' s) cmd cmd' where
--     _commands = lens commands (\s a -> s { commands = a})

-- instance HasScene (Scenario cmd s) (Scenario cmd s') s s' where
--     _scene = lens scene (\s a -> s { scene = a})

-- instance HasModel (Scenario cmd s) (Scenario cmd s') s s' where
--     _model = (lens scene (\s a -> s { scene = a})) . _model

-- editScenarioModel :: (Functor f) => LensLike' f s a -> LensLike' f (Scenario cmd s) (Scenario cmd a)
-- editScenarioModel l safa s = (\s' -> s & _scene._model .~ s' ) <$> l afa' (s ^. _scene._model)
--   where
--     afa' a = (view (_scene._model)) <$> safa (s & _scene._model .~ a)


----------------------------------------------------------------------------------

elementTarget :: ElementalId -> Traversal' (Scene s) EventTarget
elementTarget eid = _plan._elementals.ix eid._elementalRef._Just

-- sceneModel :: HasScene c c' s s' => Lens c c' s s'
-- sceneModel = _scene._model

-- -- Marks the current widget as dirty, and rerender is required
-- -- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Trigger.trigger'
-- -- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Trigger.trigger'
-- -- will not result in a rerender for the other widget.
-- dirty :: (MonadState (Scenario c s) m) => m ()
-- dirty = _scene._plan._currentFrameNum %= JE.safeModularIncrement

-- viewSubject ::
--     ( MonadReader (Entity p s) m
--     )
--     => m (Subject p)
-- viewSubject = view _subject

-- viewSelf ::
--     ( MonadReader (Entity p s) m
--     )
--     => m (ReifiedTraversal' p s)
-- viewSelf = view _self

-- viewSelf ::
--     ( HasItem (ReifiedTraversal' p s) r
--     , MonadReader r m
--     )
--     => m (ReifiedTraversal' p s)
-- viewSelf = view item

-- magnifySelf :: forall p s a r m n b proxy.
--     ( HasItem (ReifiedTraversal' p s) r
--     , HasItem (ReifiedTraversal' p a) (Replaced (ReifiedTraversal' p s) (ReifiedTraversal' p a) r)
--     , Magnify m n (Replaced (ReifiedTraversal' p s) (ReifiedTraversal' p a) r) r
--     , Contravariant (Magnified m b)
--     )
--     => proxy p -> Traversal' s a -> m b -> n b
-- magnifySelf p l = magnify (to $ item %~ go p l)
--   where
--     go :: forall p s a proxy. proxy p -> Traversal' s a -> ReifiedTraversal' p s -> ReifiedTraversal' p a
--     go _ l' (Traversal s) = Traversal (s.l')

-- magnifySelf :: forall p s a rs ra m n b proxy.
--     ( UniqueMember (ReifiedTraversal' p s) rs
--     , UniqueMember (ReifiedTraversal' p a) ra
--     , ra ~ Replace (ReifiedTraversal' p s) (ReifiedTraversal' p a) rs
--     , Magnify m n (Many ra) (Many rs)
--     , Contravariant (Magnified m b)
--     )
--     => proxy p -> Traversal' s a -> m b -> n b
-- magnifySelf p l = magnify (to $ item %~ go p l)
--   where
--     go :: forall p s a proxy. proxy p -> Traversal' s a -> ReifiedTraversal' p s -> ReifiedTraversal' p a
--     go _ l' (Traversal s) = Traversal (s.l')

