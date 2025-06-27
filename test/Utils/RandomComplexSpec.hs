module Utils.RandomComplexSpec (spec) where

import Test.Hspec
import Utils.RandomComplex
import Data.Complex
import Control.Monad (replicateM)

spec :: Spec
spec = do
  describe "randomUnitComplex" $ do
    it "generates values of unit modulus" $ do
      zs <- replicateM 100 randomUnitComplex
      all (\z -> abs (magnitude z - 1) < 1e-10) zs `shouldBe` True

  describe "randomUnitComplexList" $ do
    it "generates the correct number of unit complex numbers" $ do
      zs <- randomUnitComplexList 42
      length zs `shouldBe` 42
      all (\z -> abs (magnitude z - 1) < 1e-10) zs `shouldBe` True
