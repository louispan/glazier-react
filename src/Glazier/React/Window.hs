{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Window where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.RWS.Strict
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React.Component
import Glazier.React.Markup
import Glazier.React.ReactId
import Glazier.React.ReadIORef
import Glazier.React.Scene
import Glazier.React.Subject
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

-- The @s@ can be magnified with 'enlargeScene'
type Window s = RWST (Scene s) () (DL.DList ReactMarkup) ReadIORef

-- type SceneDisplay x s r = Display (Scene x s) r
----------------------------------------------------------------------------------

getListeners :: MonadReader (Scene s) m => ReactId -> m [JE.Property]
getListeners ri = do
    cb <- view (_plan._shimCallbacks._shimListen)
    ks <- view (_plan._elementals.ix ri._listeners.to M.keys)
    let go k = (k, bindListenerContext (context k) cb)
    pure (go <$> ks)
  where
    context k = JE.toJSR $ JA.fromList [JE.toJS ri, JE.toJS k]

-- | Interactive version of 'lf' using listeners obtained from the 'Plan' for a 'ElementalId'.
lf' :: (MonadReader (Scene s) m, MonadState (DL.DList ReactMarkup) m)
    => ReactId
    -> JE.JSRep -- ^ eg "div" or "input"
    -> DL.DList JE.Property
    -> m ()
lf' ri n props = do
    ls <- getListeners ri
    lf n (props <> DL.fromList ls)

-- | Interactive version of 'bh'
bh' :: (MonadReader (Scene s) m, MonadState (DL.DList ReactMarkup) m)
    => ReactId
    -> JE.JSRep
    -> DL.DList JE.Property
    -> m r
    -> m r
bh' ri n props childs = do
    ls <- getListeners ri
    bh n (props <> DL.fromList ls) childs

bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
bindListenerContext = js_bindListenerContext

displaySubject :: Subject s -> Window s' ()
displaySubject sbj = do
    scn <- lift (doReadIORef (sceneRef sbj))
    let ShimCallbacks renderCb renderedCb refCb _ = scn ^. _plan._shimCallbacks
    -- These are the callbacks on the 'ShimComponent'
    -- See jsbits/react.js
    lf (JE.toJSR shimComponent)
        [ ("render", JE.toJSR renderCb)
        , ("rendered", JE.toJSR renderedCb)
        , ("ref", JE.toJSR refCb)
        ]

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = function(j) { $2($1, j) };"
    js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep

#else

js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
js_bindListenerContext _ _ = JE.JSRep J.nullRef


#endif
