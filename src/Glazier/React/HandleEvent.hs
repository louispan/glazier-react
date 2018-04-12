-- | This module based on React/Flux/PropertiesAndEvents.hs.
module Glazier.React.HandleEvent where

import Control.DeepSeq

-- | Using the NFData idea from React/Flux/PropertiesAndEvents.hs
-- React re-uses Notice from a pool, which means it may no longer be valid if we lazily
-- parse it. However, we still want lazy parsing so we don't parse unnecessary fields.
-- Additionally, we don't want to block during the event handling.The reason this is a problem is
-- because Javascript is single threaded, but Haskell is lazy.
-- Therefore GHCJS threads are a strange mixture of synchronous and asynchronous threads,
-- where a synchronous thread might be converted to an asynchronous thread if a "black hole" is encountered.
-- See https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Concurrent.hs
-- This safe interface requires two input functions:
-- 1. a function to reduce Notice to a NFData. The handleEvent will ensure that the
-- NFData is forced which will ensure all the required fields from Synthetic event has been parsed.
-- This function must not block.
-- 2. a second function that uses the NFData. This function is allowed to block.
-- handleEvent results in a function that you can safely pass into 'GHC.Foreign.Callback.syncCallback1'
-- with 'GHCJS.Foreign.Callback.ContinueAsync'.
handleEvent :: NFData a => (evt -> a) -> (a -> b) -> (evt -> b)
handleEvent goStrict goLazy evt = goLazy $!! goStrict evt

-- | a monadic version of handleEvent
-- The monad's effects must not block!
handleEventM :: (Monad m, NFData a) => (evt -> m a) -> (a -> m b) -> (evt -> m b)
handleEventM goStrict goLazy evt = do
    r <- goStrict evt
    goLazy $!! r

