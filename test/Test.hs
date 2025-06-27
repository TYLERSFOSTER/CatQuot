{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Test where

import Test.Hspec
import qualified Utils.RandomComplexSpec

main :: IO ()
main = hspec $ do
  Utils.RandomComplexSpec.spec
  Utils.RandomMatrixSpec.spec
  MLGraph.FeatureSpec.spec
  MLGraph.FeatureSetSpec.spec
  MLGraph.EdgePassHomSpec.spec
