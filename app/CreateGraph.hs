module CreateGraph where

import qualified MLGraph.Feature as Feature

import Utils.RandomComplex (randomUnitComplexList)
import Utils.RandomMatrix

-- | This module defines the procedure for creating a graph
-- used in the main CatQuot executable.

generateFeatureSetIO :: Int -> IO FeatureSet
generateFeatureSetIO d = do
  randomFeature <- randomUnitComplexList d
  return [Feature.fromList randomFeature]

createNodeFeatureSet :: String -> Int -> IO NodeFeatureSet
createNodeFeatureSet n d = do
  generatedSet <- generateFeatureSetIO d
  return NodeFeatureSet
    { nodeName = n
    , ambientDimension = d
    , generate = const (return generatedSet) -- This can be pure if it's just wrapping
    , isMember = isOfDimension d
    }