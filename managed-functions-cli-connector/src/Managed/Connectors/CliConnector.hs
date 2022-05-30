{-# LANGUAGE LambdaCase #-}

module Managed.Connectors.CliConnector
  ( cliConnector
  ) where

import Data.Managed
import Data.Maybe (fromJust)
import Managed.Agent
import Managed.Probe (params, returns)
import System.Environment (getArgs)

cliConnector :: Connector SR
cliConnector = Connector {run = connect}

connect :: Agent SR -> IO ()
connect agent =
  getArgs >>= \case
    ["list"] -> do
      putStrLn "Actions:"
      printAll agent
    ["params", pid] -> print . params $ agent ! pid
    ["returns", pid] -> print . returns $ agent ! pid
    ["describe", pid] -> print $ describeHuman agent pid
    "invoke":pid:args -> invoke pid args agent >>= print
    _ -> error "Illegal arguments"

printAll :: Agent SR -> IO ()
printAll agent = mapM_ (printOne agent) $ ids agent

printOne :: Agent SR -> ProbeID -> IO ()
printOne agent = putStrLn . fromJust . describeHuman agent
