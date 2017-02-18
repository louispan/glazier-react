{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.Input where

import Control.Lens
import qualified Data.HashMap.Strict as M
import GHCJS.Types (JSString, jsval, JSVal)
import GHCJS.Marshal.Pure (PToJSVal(..))
import qualified Glazier as G
import qualified Glazier.React.Event as R
import qualified Glazier.React.Markup as R
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

data InputModel = InputModel
    { key :: JSString
    , value :: JSString
    }

makeClassy_ ''InputModel

todoInputWindow :: Monad m => G.WindowT InputModel (R.ReactMlT m) ()
todoInputWindow = do
    s <- ask
    lift $ R.leaf "input" (M.fromList
                    [ ("key", R.strProp (s ^. _key))
                    , ("className", R.strProp "new-todo")
                    , ("placeholder", R.strProp "What needs to be done?")
                    , ("value", jsval (s ^. _value))
                    , ("autoFocus", pToJSVal True)
                    ])

data InputAction = OnChange JSString

-- | unsafe to enable lazy parsing. See mkEventHandler
foreign import javascript unsafe "$1[$2]"
  js_trace :: JSVal -> IO ()


onChange :: JSVal -> IO InputAction
onChange = R.eventHandlerIO goStrict goLazy
    where
      goStrict evt = void $ runMaybeT $ do
          evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
          r <- pure . jsval . R.target . R.parseEvent $ evt'
          lift $ js_trace r

      goLazy = const $ pure $ OnChange "world"

todoInputGadget :: MonadIO io => G.GadgetT InputAction InputModel io ()
todoInputGadget = do
    a <- ask
    case a of
        OnChange str -> _value .= str


-- todoEventHandler'
-- wack :: Event -> InputAction
-- wack :: Event -> TodoInputAction
