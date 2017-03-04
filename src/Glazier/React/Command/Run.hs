-- | Common functions used by command interpreters
module Glazier.React.Command.Run where

import qualified GHCJS.Types as J

foreign import javascript unsafe
  "if ($1 && $1['setState']) { $1['setState']({ frameNum : $2 }); }"
  js_setStateFrameNum :: J.JSVal -> Int -> IO ()



-- render model frameNum = void $ runMaybeT $ do
--     s <- MaybeT $ use model
--     let s' = s & frameNum %~ (+ 1)
--     model .= Just s'

--     ms <- use
--     liftIO . void $ swapMVar ms s' -- ^ so that the render callback can use the latest state
--     let i = s' ^. TD.Todo.model . TD.Todo.frameNum
--         ref = s' ^. TD.Todo.model . TD.Todo.ref
--     liftIO $ js_setStateFrameNum ref i -- ^ notify React that the specific component has changed
