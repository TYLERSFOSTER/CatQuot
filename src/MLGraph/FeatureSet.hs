module MLGraph.FeatureSet (isOfDimension, nodeFeatureSet, NodeFeatureSet(..), nodeFeatureSet) where

import MLGraph.Feature (NodeFeature, dimension, fromList)
import Data.Complex
import qualified Data.Vector as V

-- Membership predicate
isOfDimension :: Int -> (NodeFeature -> Bool)
isOfDimension d f =
  length f == d

-- Abstract signal set representation
data NodeFeatureSet = NodeFeatureSet
  { nodeName :: String
  , ambientDimension  :: Int
  , featureSet :: [V.Vector (Complex Double)]
  , isMember :: NodeFeature -> Bool
  }

-- Equality only considers structural fields, not the isMember function
instance Eq NodeFeatureSet where
  a == b =
    nodeName a == nodeName b && 
    ambientDimension a == ambientDimension b

nodeFeatureSet :: String -> Int -> [V.Vector (Complex Double)] -> NodeFeatureSet
nodeFeatureSet n d f = 
  let thisNodeName          = n
      thisAmbientDimension  = d
      thisFeatureSet        = f
  in NodeFeatureSet
    { nodeName          = thisNodeName
    , ambientDimension  = thisAmbientDimension
    , featureSet        = thisFeatureSet
    , isMember          = isOfDimension thisAmbientDimension
    }