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
import Data.Function.Extras
import Data.STRef
import Glazier.Command
import Glazier.React.Markup
import Glazier.React.ReactPath

type ReactCont' c = AContT ()
 -- 'MonadDelegate', 'MonadDischarge'
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

-- deriving via (AContT () m) instance Monad m => MonadDelegate (ReactContT m)
deriving via (ReactCont' c) instance (Cmd' IO c, Cmd' [] c) => MonadCodify (ReactCont c)
-- deriving via (ContT () m) instance Monad m => MonadDischarge (ReactContT () m)

type instance Command (ReactCont c) = c

instance (Cmd' IO c, Cmd' [] c) => MonadDischarge (ReactCont c) where
    -- discharge (ContT g) f = lift $ g (evalContT . f)
    discharge (ReactCont (AContT g)) f = do
        ml1 <- askEnv' @Markup
        fixme $ liftIO $ putStrLn $ "A markup size " <> show (length ml1)

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

        -- at this point the markup and reactpath is not updated.
        -- it is only updated after "exec" ing the commands

        -- now make sure the bind to this monad will only fire
        -- after all commands has been evaluated
        -- by scheduling the bind to unit @() as a command
        ml2 <- askEnv' @Markup
        fixme $ liftIO $ putStrLn $ "B markup size " <> show (length ml2)

        -- we want to delegatify with the markup from running inner monad
        -- catch 22?
        -- but at the point, we don't have the markup so delegatify will
        -- capture the wrong markup.

        -- FIXME, not quite the right place to 'instruct', not quite late enough
        -- for delegateHead, it doesn't work
        delegatify $ instruct . ($ ())

        ml3 <- askEnv' @Markup
        fixme $ liftIO $ putStrLn $ "C markup size " <> show (length ml3)

        -- now read the latest state from the refs
        (liftST $ readSTRef ok) >>= guard
        (liftST $ readSTRef mlv) >>= putEnv' @Markup
        (liftST $ readSTRef rpv) >>= putEnv' @ReactPath

        -- the problem is in nested discharge
        -- the wait for () need to be applied as a bind for the inner discharge
        -- but it is an 'instruct' ?
        ml4 <- askEnv' @Markup
        fixme $ liftIO $ putStrLn $ "D markup size " <> show (length ml4)


