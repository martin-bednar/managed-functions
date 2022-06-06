{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Managed.Agent where

import GHC.IO (throwIO)
import Managed
import Managed.Exception
import Spec.Data
import Test.Hspec

spec :: Spec
spec = do
  let myIds = ["probeN", "probeB", "probeIO"]
  let myProbes = zip myIds [probeN, probeB, probeIO]
  let myAgent = fromList myProbes
  Test.Hspec.describe "list" $ do
    it "lists all probes in an Agent" $ do
      Prelude.map fst (toList myAgent) `shouldMatchList` myIds
  Test.Hspec.describe "ids" $ do
    it "lists IDs of all probes in an Agent" $ do
      ids myAgent `shouldMatchList` myIds
  Test.Hspec.describe "invoke" $ do
    it "invokes a probe with valid input" $ do
      invoke myAgent "probeB" ["A", "B"] `shouldReturn` Right "C"
    it "fails to invoke a probe with invalid input" $ do
      invoke myAgent "probeB" ["A", "X"] `shouldReturn` Left NoParseArgument
    it "fails to invoke a nonexistent probe" $ do
      invoke myAgent "probeX" [] `shouldReturn` Left (BadProbeID "probeX")
    it "detect wrong number of input parameters" $ do
      invoke myAgent "probeB" ["A", "B", "C"] `shouldReturn`
        Left (BadNumberOfArguments 2 3)
    it "catches any error thrown inside a Probe" $ do
      invoke errAgent "simpleErr" [] >>= (`shouldSatisfy` isRuntimeException)
      invoke errAgent "ioErr" [] >>= (`shouldSatisfy` isRuntimeException)

errAgent :: Agent SR
errAgent =
  fromList
    [ ("simpleErr", toProbe (error "Bad Error!" :: String))
    , ("ioErr", toProbe ((throwIO $ userError "Bad Error!") :: IO String))
    ]

isRuntimeException :: Either AgentException b -> Bool
isRuntimeException (Left (ProbeRuntimeException _)) = True
isRuntimeException _ = False
