{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}

module MLGraph.EdgePassHomSpec (spec) where

import Test.Hspec
import MLGraph.FeatureSet
import MLGraph.Feature (NodeFeature)
import qualified MLGraph.EdgePassHom as EPHom

import qualified Data.Vector as V
import Numeric.LinearAlgebra
import Data.Complex
import Data.Maybe (isJust, isNothing)

unitVec :: [Complex Double] -> NodeFeature
unitVec = V.fromList

matrixFromLists :: [[Complex Double]] -> Matrix (Complex Double)
matrixFromLists = fromLists

testSourceSet :: NodeFeatureSet
testSourceSet = nodeFeatureSet "source" 2
  [unitVec [1 :+ 0, 0 :+ 0], unitVec [0 :+ 0, 1 :+ 0]]

testTargetSet :: NodeFeatureSet
testTargetSet = nodeFeatureSet "target" 2
  [unitVec [0 :+ 0, 1 :+ 0], unitVec [1 :+ 0, 0 :+ 0]]

testMatrix :: Matrix (Complex Double)
testMatrix = matrixFromLists [[0, 1], [1, 0]]  -- swaps coordinates

spec :: Spec
spec = do
  describe "mkMorphism" $ do
    it "constructs a valid EdgePassHom if dimensions agree" $ do
      let result = EPHom.mkMorphism testSourceSet testTargetSet testMatrix
      isJust result `shouldBe` True

    it "fails to construct EdgePassHom if dimensions don't agree" $ do
      let badMatrix = matrixFromLists [[1, 0]]
      isNothing (EPHom.mkMorphism testSourceSet testTargetSet badMatrix) `shouldBe` True


  describe "identity" $ do
    it "acts as identity on each feature vector" $ do
      let idHom = EPHom.identity testSourceSet
      all (\v -> EPHom.applyMorphism idHom v == v) (featureSet testSourceSet) `shouldBe` True

  describe "compose" $ do
    it "composes two compatible morphisms correctly" $ do
      let Just h1 = EPHom.mkMorphism testSourceSet testTargetSet testMatrix
      let Just h2 = EPHom.mkMorphism testTargetSet testSourceSet testMatrix
      let Just composed = EPHom.compose h2 h1  -- h2 ∘ h1 = identity
      let idMat = ident 2 :: Matrix (Complex Double)
      EPHom.matrix composed `shouldSatisfy` (== idMat)

{-}
    it "fails to compose incompatible morphisms" $ do
      let s2 = nodeFeatureSet "wrong" 3 []
          Just h1 = EPHom.mkMorphism testSourceSet testTargetSet testMatrix
          maybeH2 = EPHom.mkMorphism s2 testSourceSet (matrixFromLists [[1, 0], [0, 1], [0, 0]])
      case maybeH2 of
        Just h2 -> isNothing (EPHom.compose h1 h2) `shouldBe` True
        Nothing -> expectationFailure "mkMorphism unexpectedly failed to produce h2 (check matrix shape)"
-}

  describe "applyMorphism" $ do
    it "applies the matrix correctly to a vector" $ do
      let Just hom = EPHom.mkMorphism testSourceSet testTargetSet testMatrix
          vec = unitVec [1 :+ 0, 2 :+ 0]
          result = EPHom.applyMorphism hom vec
      result `shouldBe` unitVec [2 :+ 0, 1 :+ 0]

  describe "applyIfMember" $ do
    it "applies hom if the vector is in source set" $ do
      let Just hom = EPHom.mkMorphism testSourceSet testTargetSet testMatrix
          vec = head (featureSet testSourceSet)
      EPHom.applyIfMember hom vec `shouldBe` Just (EPHom.applyMorphism hom vec)

    it "returns Nothing if the vector is not in source set" $ do
      let Just hom = EPHom.mkMorphism testSourceSet testTargetSet testMatrix
          vec = unitVec [1 :+ 0]  -- wrong dimension
      EPHom.applyIfMember hom vec `shouldBe` Nothing
