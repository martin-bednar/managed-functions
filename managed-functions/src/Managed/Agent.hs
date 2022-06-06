{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Managed.Agent
  ( fromList
  , toList
  , (!)
  , (!?)
  , invoke
  , ids
  , invokeUnsafe
  , describe
  , describeEither
  , describeHuman
  ) where

import qualified Data.Map as M

import Data.Managed

import Control.DeepSeq (NFData, force)
import Control.Exception (catch, evaluate, toException)
import Control.Monad.Catch (try)
import Managed.Exception
import Managed.ProbeDescription
import System.IO.Error (catchIOError)

-- | 'Data.Map.fromList' specialized to 'Agent'
fromList :: [(ProbeID, Probe e)] -> Agent e
fromList = M.fromList

-- | 'Data.Map.toList' specialized to 'Agent'
toList :: Agent e -> [(ProbeID, Probe e)]
toList = M.toList

infixl 9 !, !?

-- | 'Data.Map.!' specialized to 'Agent'
(!) :: Agent e -> ProbeID -> Probe e
(!) = (M.!)

-- | 'Data.Map.!?' specialized to 'Agent'
(!?) :: Agent e -> ProbeID -> Maybe (Probe e)
(!?) = (M.!?)

-- | Invokes (calls) a 'Probe'.
--
-- This function never throws an exception,
-- instead, an 'Either' is used.
-- Any errors and exceptions caused by incorrect 'ProbeID',
-- incorrect input parameters, or by the probe call itself
-- are caught and passed as an 'AgentException'.
--
-- The result is fully evaluated using 'force'
-- before it's returned
-- (to keep all exceptions inside the 'Either').
invoke ::
     (NFData (Out e))
  => Agent e -- ^ Agent that contains the probe to be called
  -> ProbeID -- ^ ID of the probe to be called
  -> [In e] -- ^ Input parameters
  -> IO (Either AgentException (Out e))
invoke a p i = try $ invokeUnsafe a p i

-- | An unsafe variant of 'invoke'.
--
-- This function rethrows all exceptions and errors
-- caused by probe lookup or input parameters.
-- Exceptions caused by probe invocation
-- are rethrown as 'ProbeRuntimeException'.
invokeUnsafe ::
     (NFData (Out e))
  => Agent e
  -> ProbeID
  -> [In e]
  -> IO (Out e)
invokeUnsafe agent pid input =
  findOrThrow agent pid >>= callStrict input

-- | List all Probe IDs
ids :: Agent e -> [ProbeID]
ids = Prelude.map fst . toList

-- | Create a full description of a 'Probe'
describe :: Agent e -> ProbeID -> Maybe ProbeDescription
describe agent pid = mkDescription pid <$> agent !? pid

-- | A variant of 'describe' that returns 'Either' instead of 'Maybe'
describeEither ::
     Agent e
  -> ProbeID
  -> Either AgentException ProbeDescription
describeEither agent pid =
  case describe agent pid of
    Nothing -> Left $ badProbeID pid
    Just res -> Right res

-- | Create a human-readable description of a probe
describeHuman :: Agent e -> ProbeID -> Maybe String
describeHuman agent pid = human <$> describe agent pid

-- Utility functions
findOrThrow :: Agent e -> ProbeID ->  IO (Probe e)
findOrThrow agent pid =
  case agent !? pid of
    Nothing -> throwM $ badProbeID pid
    Just probe -> return probe

callStrict ::
     (NFData (Out e)) => [In e] -> Probe e -> IO (Out e)
callStrict input p = callOrThrow input p >>= evalOrThrow

callOrThrow :: [In e] -> Probe e -> IO (Out e)
callOrThrow input p =
  call p input `catchIOError`
  (throwM . probeRuntimeException . toException)

evalOrThrow :: (NFData a) => a -> IO a
evalOrThrow x =
  (evaluate . force) x `catch`
  (throwM . probeRuntimeException)
