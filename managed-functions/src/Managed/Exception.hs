{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Managed.Exception
  ( AgentException(..)
  , throwM
  , explain
  , badNumberOfArgs
  , noParseArg
  , probeRuntimeException
  , badProbeID
  ) where

import Control.Exception (Exception(..), SomeException)
import Control.Monad.Catch (throwM)
import Data.Managed
import GHC.Generics (Generic)

data AgentException
  = BadProbeID ProbeID
  | BadNumberOfArguments Int Int
  | NoParseArgument
  | ProbeRuntimeException String
  deriving (Show, Eq, Generic)

instance Exception AgentException where
  displayException = explain

explain :: AgentException -> String
explain (BadProbeID pid) = "Unrecognized ProbeID: " ++ show pid
explain (BadNumberOfArguments expected got) =
  "Bad number of arguments. Expected " ++
  show expected ++ ", but got " ++ show got
explain NoParseArgument = "Can't parse probe input argument."
explain (ProbeRuntimeException reason) =
  "Exception thrown in probe invocation:\n" ++ reason

badNumberOfArgs :: Int -> [a] -> AgentException
badNumberOfArgs n xs = BadNumberOfArguments n (length xs)

noParseArg :: AgentException
noParseArg = NoParseArgument

probeRuntimeException :: SomeException -> AgentException
probeRuntimeException exception = ProbeRuntimeException (show exception)

badProbeID :: ProbeID -> AgentException
badProbeID = BadProbeID
