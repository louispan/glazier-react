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
import Control.Monad.State.Lazy as Lazy
import Control.Monad.Trans.ACont
import Control.Monad.Trans.Maybe
import Data.STRef
import Glazier.Command
import Glazier.React.Markup
import Glazier.React.ReactPath

type ReactCont' c = AContT () -- 'MonadDelegate', 'MonadDischarge'
    (MaybeT -- 'Alternative'
    -- State monads must be inside ContT to be a 'MonadDelegate'
    (Lazy.StateT ReactPath -- 'PutReactPath'x
    (Lazy.StateT Markup -- 'PutMarkup'
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
        , MonadPut' ReactPath
        , MonadPut' Markup
        , MonadDelegate
        )

deriving via (ReactCont' c) instance (Cmd' IO c, Cmd' [] c) => MonadCodify (ReactCont c)

type instance Command (ReactCont c) = c

-- | This instance 'MonadDischarge' ensures the @()@ is fired
-- after the 'MonadProgram' commands are evaluated.
instance (Cmd' IO c, Cmd' [] c) => MonadDischarge (ReactCont c) where
    discharge m f = do
        -- Warning, the effects (Markup, ReactPath) inside ContT
        -- and above ProgramT are not the latest version after discharge,
        -- since 'codify'ed handlers are evaluated later.
        -- So we need to save the latest state (Markup and ReactPath)
        -- from running the 'codify'ed handlers.
        ok <- liftST $ newSTRef True
        mlv <- getEnv' @Markup >>= liftST . newSTRef
        rpv <- getEnv' @ReactPath >>= liftST . newSTRef


        let f' a = do
                f a <|> resetGuard
                liftST $ writeSTRef ok True
                getEnv' @Markup >>= liftST . writeSTRef mlv
                getEnv' @ReactPath >>= liftST . writeSTRef rpv

        -- This doens't work!
        -- let finally' = delegate $ \fire -> do
        --         fire () <|> resetGuard
        --         liftST $ writeSTRef ok True
        --         getEnv' @Markup >>= liftST . writeSTRef mlv
        --         getEnv' @ReactPath >>= liftST . writeSTRef rpv
            resetGuard = do
                liftST $ writeSTRef ok False
                empty
            m' = do
                -- finally'
                m
            (ReactCont (AContT g)) = m'

        ReactCont $ lift $ g (evalAContT . unReactCont . f')
        -- ReactCont $ lift $ g (evalAContT . unReactCont . f)

        -- now make sure the bind to this monad will only fire
        -- after all commands has been evaluated
        -- by scheduling the bind to unit @() as a command
        delegatify $ instruct . ($ ())

        -- now read the latest state from the refs
        (liftST $ readSTRef ok) >>= guard
        (liftST $ readSTRef mlv) >>= putEnv' @Markup
        (liftST $ readSTRef rpv) >>= putEnv' @ReactPath


