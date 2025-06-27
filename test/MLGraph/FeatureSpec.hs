module MLGraph.FeatureSpec (spec) where

import Test.Hspec
import MLGraph.Feature
import Data.Complex

spec :: Spec
spec = do
  describe "NodeFeature" $ do
    it "round-trips from list to vector and back" $ do
      let xs = [1 :+ 0, 2 :+ 1, 3 :+ (-1)]
      toList (fromList xs) `shouldBe` xs

    it "computes the correct dimension" $ do
      let nf = fromList [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0]
      dimension nf `shouldBe` 4
