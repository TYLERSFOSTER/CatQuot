module MLGraph.FeatureSet (isOfDimension, nodeFeatureSet, NodeFeatureSet(..), nodeFeatureSet) where

import Utils.RandomComplex (randomUnitComplexList)
import MLGraph.Feature (NodeFeature, dimension, fromList)
import Data.Complex
import qualified Data.Vector as V

-- Membership predicate
isOfDimension :: Int -> (NodeFeature -> Bool)
isOfDimension d f =
  length f == d

-- Finite sampling generator
generateFeatureSet :: Int -> IO (V.Vector (Complex Double))
generateFeatureSet d = do
  xs <- randomUnitComplexList d
  return $ V.fromList xs

-- Abstract signal set representation
data NodeFeatureSet = NodeFeatureSet
  { nodeName :: String
  , ambientDimension  :: Int
  , generate :: IO (V.Vector (Complex Double))
  , isMember :: NodeFeature -> Bool
  }

-- Equality only considers structural fields, not the isMember function
instance Eq NodeFeatureSet where
  a == b =
    nodeName a == nodeName b && 
    ambientDimension a == ambientDimension b

nodeFeatureSet :: String -> Int -> NodeFeatureSet
nodeFeatureSet n d = 
  let thisNodeName          = n
      thisAmbientDimension  = d
  in NodeFeatureSet
    { nodeName          = thisNodeName
    , ambientDimension  = thisAmbientDimension
    , generate          = generateFeatureSet thisAmbientDimension
    , isMember          = isOfDimension thisAmbientDimension
    }
