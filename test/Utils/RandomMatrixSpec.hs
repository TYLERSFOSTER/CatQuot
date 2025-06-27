module Utils.RandomMatrixSpec (spec) where

import Test.Hspec
import Utils.RandomMatrix
import Utils.RandomComplex (randomUnitComplex)
import Numeric.LinearAlgebra
import Data.Complex

spec :: Spec
spec = do
  describe "randomUnitComplexHMatrix" $ do
    it "produces a square matrix of the correct size with unit modulus entries" $ do
      let d = 4
      m <- randomUnitComplexHMatrix d
      let numRows = rows m
          numCols = cols m
          allUnit = all (\z -> abs (magnitude z - 1) < 1e-10) (toList $ flatten m)
      numRows `shouldBe` d
      numCols `shouldBe` d
      allUnit `shouldBe` True

  describe "multiplyMatrixVector" $ do
    it "performs matrix-vector multiplication correctly for known inputs" $ do
      let m = (2><2) [1 :+ 0, 2 :+ 0,
                      3 :+ 0, 4 :+ 0] :: Matrix (Complex Double)
          v = fromList [1 :+ 0, 1 :+ 0]
          expected = fromList [3 :+ 0, 7 :+ 0]
      multiplyMatrixVector m v `shouldBe` expected
