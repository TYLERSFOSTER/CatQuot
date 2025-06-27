module MLGraph.FeatureSetSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import MLGraph.FeatureSet
import MLGraph.Feature
import Data.Complex
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "isOfDimension" $ do
    let testCases =
          [ (3, fromList [1 :+ 0, 2 :+ 0, 3 :+ 0], True)
          , (2, fromList [1 :+ 0, 2 :+ 0, 3 :+ 0], False)
          , (0, fromList [], True)
          , (1, fromList [], False)
          ]

    forM_ testCases $ \(d, feature, expected) ->
      it ("returns " ++ show expected ++ " for dimension " ++ show d ++ " and input of length " ++ show (dimension feature)) $
        isOfDimension d feature `shouldBe` expected

  describe "nodeFeatureSet" $ do
    let name = "test-node"
        dim  = 2
        vecs = [V.fromList [1 :+ 0, 2 :+ 0], V.fromList [0 :+ 1, 0 :+ (-1)]]
        nfs  = nodeFeatureSet name dim vecs

    it "constructs a NodeFeatureSet with correct metadata" $ do
      nodeName nfs `shouldBe` name
      ambientDimension nfs `shouldBe` dim
      featureSet nfs `shouldBe` vecs

    it "has equality that ignores the isMember function" $ do
      let nfs2 = nodeFeatureSet name dim []
      nfs `shouldBe` nfs2
