module Main where

import Spec.Managed.Agent
import Spec.Managed.Probe
import Spec.Managed.Probe.ToProbe
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe
      "Managed.Probe.ToProbe"
      Spec.Managed.Probe.ToProbe.spec
    describe "Managed.Probe" Spec.Managed.Probe.spec
    describe "Managed.Agent" Spec.Managed.Agent.spec
