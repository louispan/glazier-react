{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.ReactCont where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.ST.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.ACont
import Control.Monad.Trans.Maybe
import qualified Data.DList as DL
import Data.STRef
import Glazier.Command
import Glazier.React.Markup
import Glazier.React.ReactPath

type ReactCont' c = AContT () -- 'MonadDelegate', 'MonadDischarge'
    (MaybeT -- 'Alternative'
    -- State monads must be inside ContT to be a 'MonadDelegate'
    (StateT ReactPath -- 'PutReactPath', 'AskReactPath'
    (StateT (DL.DList ReactMarkup) -- 'PutMarkup'
    (ProgramT c IO -- 'MonadCodify', 'MonadProgram', 'MonadST', 'MonadIO'
    ))))

newtype ReactCont c a = ReactCont { unReactCont :: ReactCont' c a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , MonadPlus
        , Monad
        , MonadIO
        , MonadST
        , MonadCont
        , MonadProgram
        , MonadAsk' ReactPath
        , MonadPut' ReactPath
        , MonadAsk' Markup
        , MonadPut' Markup
        , MonadDelegate
        )

deriving via (ReactCont' c) instance (Cmd' IO c, Cmd' [] c) => MonadCodify (ReactCont c)

type instance Command (ReactCont c) = c

-- | This instance 'MonadDischarge' ensures the @()@ is fired
-- after the 'MonadProgram' commands are evaluated
-- and that the 'Markup' and 'ReactPath' and 'MaybeT' state is restored.
instance (Cmd' IO c, Cmd' [] c) => MonadDischarge (ReactCont c) where
    discharge (ReactCont (AContT g)) f = do
        ok <- liftST $ newSTRef True
        mlv <- askEnv' @Markup >>= liftST . newSTRef
        rpv <- askEnv' @ReactPath >>= liftST . newSTRef

        let f' a = do
                f a <|> saveGuard
                askEnv' @Markup >>= liftST . writeSTRef mlv
                askEnv' @ReactPath >>= liftST . writeSTRef rpv

            saveGuard = do
                liftST $ writeSTRef ok False
                empty

        ReactCont $ lift $ g (evalAContT . unReactCont . f')

        -- now make sure the bind to this monad will only fire
        -- after all commands has been evaluated
        -- by scheduling the bind to unit @() as a command
        delegatify $ instruct . ($ ())

        -- now read the latest state from the refs
        (liftST $ readSTRef ok) >>= guard
        (liftST $ readSTRef mlv) >>= putEnv' @Markup
        (liftST $ readSTRef rpv) >>= putEnv' @ReactPath


