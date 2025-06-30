{-# LANGUAGE OverloadedStrings #-}

module MLGraph.MessPassDiagSpec where

import Test.Hspec
import MLGraph.MessPassDiag
import MLGraph.EdgePassHom
import MLGraph.FeatureSet

import Data.Complex (Complex(..))
import qualified Data.Vector as V
import Numeric.LinearAlgebra (Matrix, ident)

-- Helper: Create a dummy NodeFeatureSet with the given name and dim
mkNode :: String -> Int -> NodeFeatureSet
mkNode name d = nodeFeatureSet name d [V.replicate d (0 :+ 0)]

-- Helper: Create a simple morphism between two nodes
mkHom :: String -> NodeFeatureSet -> NodeFeatureSet -> EdgePassHom
mkHom name src tgt = EdgePassHom
  { edgeName = name
  , source = src
  , target = tgt
  , matrix = ident (ambientDimension src)
  , fiberEdges = []
  }

spec :: Spec
spec = do
  describe "nodeNames" $ do
    it "extracts all node names from a MessPassGraph" $ do
      let n1 = mkNode "U" 2
          n2 = mkNode "V" 3
      nodeNames (MessPassGraph [n1, n2] []) `shouldBe` ["U", "V"]

  describe "validateGraph" $ do
    it "returns True when all morphisms are between existing nodes" $ do
      let n1 = mkNode "A" 2
          n2 = mkNode "B" 2
          m  = mkHom "f" n1 n2
      validateGraph (MessPassGraph [n1, n2] [m]) `shouldBe` True

    it "returns False when morphism source is missing" $ do
      let n1 = mkNode "A" 2
          n2 = mkNode "B" 2
          n3 = mkNode "C" 2
          m  = mkHom "bad" n3 n1
      validateGraph (MessPassGraph [n1, n2] [m]) `shouldBe` False

    it "returns False when morphism target is missing" $ do
      let n1 = mkNode "A" 2
          n2 = mkNode "B" 2
          n3 = mkNode "C" 2
          m  = mkHom "bad" n1 n3
      validateGraph (MessPassGraph [n1, n2] [m]) `shouldBe` False
