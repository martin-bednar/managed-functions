{-# LANGUAGE TypeApplications #-}

module Spec.Managed.Probe where

import Data.Typeable (Proxy(Proxy), TypeRep, typeRep)
import Managed.Probe
import Spec.Data
import Test.Hspec

myData :: TypeRep
myData = typeRep (Proxy @MyData)

spec :: Spec
spec = do
  describe "returns" $ do
    it "Finds the return type of a Probe" $ do
      returns probeN `shouldBe` myData
      returns probeIO `shouldBe` typeRep (Proxy @(IO MyData))
  describe "params" $ do
    it "Finds the param types of a Probe" $ do
      params probeN `shouldBe` []
      params probeIntCharStr `shouldBe` [int, char]
