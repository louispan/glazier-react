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
import Glazier.Benign
import Glazier.React.Model
import Glazier.React.Obj
import qualified JavaScript.Extras as JE

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- The @s@ can be magnified with 'magnifiedModel'
type Window s = RWST (Model s) () (DL.DList ReactMarkup) (Benign IO)

-- type ModelDisplay x s r = Display (Model x s) r
----------------------------------------------------------------------------------

getListeners :: MonadReader (Model s) m => ReactId -> m [JE.Property]
getListeners k = do
    ls <- view (_plan._elementals.ix k._reactListeners.to M.toList)
    pure $ (\(n, (cb, _)) -> (n, JE.toJSR cb)) <$> ls

-- | Interactive version of 'lf' using listeners obtained from the 'Plan' for a 'ElementalId'.
lf' :: (MonadReader (Model s) m, MonadState (DL.DList ReactMarkup) m)
    => ReactId
    -> JE.JSRep -- ^ eg "div" or "input"
    -> DL.DList JE.Property
    -> m ()
lf' k n props = do
    ls <- getListeners k
    lf n (props <> DL.fromList ls)

-- | Interactive version of 'bh'
bh' :: (MonadReader (Model s) m, MonadState (DL.DList ReactMarkup) m)
    => ReactId
    -> JE.JSRep
    -> DL.DList JE.Property
    -> m r
    -> m r
bh' k n props childs = do
    ls <- getListeners k
    bh n (props <> DL.fromList ls) childs

bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
bindListenerContext = js_bindListenerContext

displayObj :: (MonadTrans t, MonadIO m, MonadState (DL.DList ReactMarkup) (t (Benign m))) => Obj s -> t (Benign m) ()
displayObj obj = do
    scn <- lift (benignReadIORef (modelRef obj))
    let scb = scn ^. _plan._shimCallbacks
        renderCb = shimRender scb
        mountedCb = shimMounted scb
        renderedCb = shimRendered scb
        refCb = shimRef scb
        k = scn ^. _plan._planId
    -- These are the callbacks on the 'ShimComponent'
    -- See jsbits/react.js

    lf (JE.toJSR shimComponent)
        [ ("render", JE.toJSR renderCb)
        , ("mounted", JE.toJSR mountedCb)
        , ("rendered", JE.toJSR renderedCb)
        , ("ref", JE.toJSR refCb)
        , ("key", reactIdKey' k)
        ]

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = function(j) { $2($1, j) };"
    js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep

#else

js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
js_bindListenerContext _ _ = JE.JSRep J.nullRef


#endif
