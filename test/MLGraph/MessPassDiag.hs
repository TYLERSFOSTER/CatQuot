{-# LANGUAGE OverloadedLists #-}

module MLGraph.MessPassDiagSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V
import Data.Complex (Complex((:+)))
import Numeric.LinearAlgebra (ident)
import MLGraph.MessPassDiag
import MLGraph.FeatureSet
import MLGraph.EdgePassHom

spec :: Spec
spec = describe "MessPassGraph" $ do

  let fsA = nodeFeatureSet "A" 3 [V.fromList [1 :+ 0, 0, 0]]
      fsB = nodeFeatureSet "B" 3 [V.fromList [0, 1 :+ 0, 0]]
      fsC = nodeFeatureSet "C" 3 [V.fromList [0, 0, 1 :+ 0]]

      eAB = EdgePassHom fsA fsB (ident 3)
      eBC = EdgePassHom fsB fsC (ident 3)
      eXX = EdgePassHom fsA fsA (ident 3) -- for reflexivity test

  it "extracts node names correctly" $ do
    let g = MessPassGraph [fsA, fsB, fsC] [eAB, eBC]
    nodeNames g `shouldMatchList` ["A", "B", "C"]

  it "validates when all morphism endpoints are in g0" $ do
    let g = MessPassGraph [fsA, fsB, fsC] [eAB, eBC, eXX]
    validateGraph g `shouldBe` True

  it "fails validation when morphism source is not in g0" $ do
    let fake = nodeFeatureSet "Z" 3 [V.fromList [9 :+ 0, 9, 9]]
        badEdge = EdgePassHom fake fsA (ident 3)
        g = MessPassGraph [fsA, fsB, fsC] [badEdge]
    validateGraph g `shouldBe` False

  it "fails validation when morphism target is not in g0" $ do
    let fake = nodeFeatureSet "Z" 3 [V.fromList [9 :+ 0, 9, 9]]
        badEdge = EdgePassHom fsA fake (ident 3)
        g = MessPassGraph [fsA, fsB, fsC] [badEdge]
    validateGraph g `shouldBe` False
