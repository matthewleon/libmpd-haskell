{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wwarn #-}

-- |
-- Module      : StringConn
-- Copyright   : (c) Ben Sinclair 2005-2009
-- License     : MIT (see LICENSE)
-- Stability   : alpha
--
-- A testing scaffold for MPD commands

module StringConn where

import           Control.Applicative
import           Control.Exception.Safe (MonadCatch, MonadThrow, throw)
import           Prelude hiding (exp)
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.MPD.Core

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8

-- | An expected request.
type Expect = String

data StringMPDError
    = TooManyRequests
    | UnexpectedRequest Expect String
      deriving (Show, Eq)

data Result a
    = Ok
    | BadResult a a  -- expected, then actual
    | BadRequest StringMPDError
      deriving (Show, Eq)

newtype StringMPD a =
    SMPD { runSMPD :: (StateT [(Expect, String)]
                       (ReaderT Password IO)) a
         } deriving (Functor, Applicative, Monad, MonadCatch, MonadThrow)

instance MonadMPD StringMPD where
    getVersion  = error "StringConn.getVersion: undefined"
    setPassword = error "StringConn.setPassword: undefined"

    open  = return ()
    close = return ()
    getPassword = SMPD ask
    send request =
        SMPD $ do
            ~pairs@((expected_request,response):rest) <- get
            when (null pairs)
                 (throw $ Custom "too many requests")
            when (expected_request /= request)
                 (throw . Custom $ "unexpected request: " ++ show request ++ ", expected: " ++ show expected_request)
            put rest
            return . B.lines $ UTF8.fromString response

testMPD :: (Eq a) => [(Expect, String)] -> StringMPD a -> IO a
testMPD pairs m = testMPDWithPassword pairs "" m

-- | Run an action against a set of expected requests and responses,
-- and an expected result. The result is Nothing if everything matched
-- what was expected. If anything differed the result of the
-- computation is returned along with pairs of expected and received
-- requests.
testMPDWithPassword :: (Eq a)
        => [(Expect, String)] -- ^ The expected requests and their
                                       -- ^ corresponding responses.
        -> Password                    -- ^ A password to be supplied.
        -> StringMPD a                 -- ^ The MPD action to run.
        -> IO a
testMPDWithPassword pairs passwd m = runReaderT (evalStateT (runSMPD m) pairs) passwd
