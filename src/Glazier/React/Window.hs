{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Window where

import Control.Lens
import Control.Monad.Trans.ARWS.Strict
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React.Component
import Glazier.React.Markup
import Glazier.React.MkId
import Glazier.React.Scene
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

type WindowT s m = ARWST (Scene s) () (DL.DList ReactMarkup) m
type Window s = WindowT s Identity

-- type SceneDisplay x s r = Display (Scene x s) r
----------------------------------------------------------------------------------

getListeners :: Monad m => GizmoId -> WindowT s m [JE.Property]
getListeners gid = do
    cb <- preview (_plan._shimCallbacks._Just._onListen)
    case cb of
        Nothing -> pure mempty
        Just cb' -> do
            ks <- view (_plan._gizmos.ix gid._listeners.to M.keys)
            let go k = (k, bindListenerContext (context k) cb')
            pure (go <$> ks)
  where
    context k = JE.toJSR $ JA.fromList [JE.toJS gid, JE.toJS k]

-- | Interactive version of 'lf' using listeners obtained from the 'Plan' for a 'GizmoId'.
lf' ::
    Monad m
    => GizmoId
    -> JE.JSRep -- ^ eg "div" or "input"
    -> DL.DList JE.Property
    -> WindowT s m ()
lf' gid n props = do
    ls <- getListeners gid
    leaf n (props <> DL.fromList ls)

-- | Interactive version of 'bh'
bh' ::
    Monad m
    => GizmoId
    -> JE.JSRep
    -> DL.DList JE.Property
    -> WindowT s m r
    -> WindowT s m r
bh' gid n props childs = do
    ls <- getListeners gid
    branch n (props <> DL.fromList ls) childs

-- | Use this to create a display for a top level 'Gadget'
-- Eg. the result of a 'Widget' that has the Window rendering function
-- inserted into 'Glazier.React.Framework.Core.Widget.MkShimListeners'.
shimWindow :: Monad m => WindowT s m ()
shimWindow = do
    ls <- view (_plan._shimCallbacks)
    case ls of
        Nothing -> pure ()
        Just (ShimCallbacks renderCb updatedCb refCb _) ->
            -- These are the callbacks on the 'ShimComponent'
            -- See jsbits/react.js
            leaf shimComponent
                [ ("render", JE.toJSR renderCb)
                , ("updated", JE.toJSR updatedCb)
                , ("ref", JE.toJSR refCb)
                ]


bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
bindListenerContext = js_bindListenerContext

#ifdef __GHCJS__

-- -- | Combine functions into a single function
-- -- Given two 'Callback (JSVal -> IO ())'
-- -- return a function that calls both callbacks
-- foreign import javascript unsafe
--     "$r = function(j) { $1(j); $2(j); };"
--     js_combineFunction1 ::
--         JE.JSRep
--         -> JE.JSRep
--         -> JE.JSRep

foreign import javascript unsafe
    "$r = function(j) { $2($1, j) };"
    js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep

#else

-- js_combineFunction1 ::
--     JE.JSRep
--     -> JE.JSRep
--     -> JE.JSRep
-- js_combineFunction1 = const

js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
js_bindListenerContext _ _ = JE.JSRep J.nullRef


#endif
