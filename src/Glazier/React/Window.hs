{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
import Glazier.React.Markup
import Glazier.React.MkId
import Glazier.React.Scene
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

type WindowT s m = ARWST (Scene s) () (DL.DList ReactMarkup) m
type Window s = WindowT s Identity

-- type SceneDisplay x s r = Display (Scene x s) r
----------------------------------------------------------------------------------

getListeners :: Monad m => ElementalId -> WindowT s m [JE.Property]
getListeners eid = do
    cb <- view (_plan._shimCallbacks._shimListen)
    ks <- view (_plan._elementals.ix eid._listeners.to M.keys)
    let go k = (k, bindListenerContext (context k) cb)
    pure (go <$> ks)
  where
    context k = JE.toJSR $ JA.fromList [JE.toJS eid, JE.toJS k]

-- | Interactive version of 'lf' using listeners obtained from the 'Plan' for a 'ElementalId'.
lf' ::
    Monad m
    => ElementalId
    -> JE.JSRep -- ^ eg "div" or "input"
    -> DL.DList JE.Property
    -> WindowT s m ()
lf' eid n props = do
    ls <- getListeners eid
    leaf n (props <> DL.fromList ls)

-- | Interactive version of 'bh'
bh' ::
    Monad m
    => ElementalId
    -> JE.JSRep
    -> DL.DList JE.Property
    -> WindowT s m r
    -> WindowT s m r
bh' eid n props childs = do
    ls <- getListeners eid
    branch n (props <> DL.fromList ls) childs


bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
bindListenerContext = js_bindListenerContext

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = function(j) { $2($1, j) };"
    js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep

#else

js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
js_bindListenerContext _ _ = JE.JSRep J.nullRef


#endif
