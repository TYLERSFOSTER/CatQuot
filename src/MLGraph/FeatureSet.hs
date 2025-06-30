module MLGraph.FeatureSet (isOfDimension, nodeFeatureSet, NodeFeatureSet(..)) where

import MLGraph.Feature (NodeFeature)
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
  , fiberNodes :: [String]
  }

-- Equality only considers structural fields, not the isMember function
instance Eq NodeFeatureSet where
  a == b =
    nodeName a == nodeName b && 
    ambientDimension a == ambientDimension b

-- OUTSIDE the data declaration
instance Show NodeFeatureSet where
  show nfs = "NodeFeatureSet { nodeName = "
          ++ show (nodeName nfs)
          ++ ", ambientDimension = "
          ++ show (ambientDimension nfs)
          ++ ", featureSet = "
          ++ show (featureSet nfs)
          ++ ", isMember = <function> }"

-- Smart constructor
nodeFeatureSet :: String -> Int -> [V.Vector (Complex Double)] -> NodeFeatureSet
nodeFeatureSet n d f = 
  let thisNodeName          = n
      thisAmbientDimension  = d
      thisFeatureSet        = f
  in NodeFeatureSet
    { nodeName         = thisNodeName
    , ambientDimension = thisAmbientDimension
    , featureSet       = thisFeatureSet
    , isMember         = isOfDimension thisAmbientDimension
    , fiberNodes       = []
    }