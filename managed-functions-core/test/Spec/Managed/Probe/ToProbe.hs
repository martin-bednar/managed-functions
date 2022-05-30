module Spec.Managed.Probe.ToProbe where

import Data.Typeable (typeOf)
import Managed hiding (describe)
import Managed.Exception
import Spec.Data
import Test.Hspec

spec :: Spec
spec = do
  describe "toProbe" $ do
    it "converts a nullary function (constant) to a Probe" $ do
      call probeN [] >>= (`shouldBe` show A)
    it "converts a unary function to a Probe" $ do
      call probeU [show B] >>= (`shouldBe` show B)
    it "converts a binary function to a Probe" $ do
      call probeB [show A, show B] >>= (`shouldBe` show C)
    it "converts an IO function to a Probe" $ do
      call probeIO [show A] >>= (`shouldBe` show A)
    it "rejects wrong number of parameters" $ do
      call probeN [show B] `shouldThrow`
        (== BadNumberOfArguments 0 1)
      call probeU [] `shouldThrow`
        (== BadNumberOfArguments 1 0)
      call probeU [show B, show B] `shouldThrow`
        (== BadNumberOfArguments 1 2)
      call probeB [show B] `shouldThrow`
        (== BadNumberOfArguments 2 1)
    it "creates the correct typeRep" $ do
      typeRep probeB `shouldBe` typeOf binary
      typeRep probeIO `shouldBe` typeOf unaryIO
