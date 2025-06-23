module MLGraph.Feature (NodeFeature, dimension, fromList, toList) where

import Data.Complex (Complex)
import qualified Data.Vector as V

type NodeFeature = V.Vector (Complex Double)

dimension :: NodeFeature -> Int
dimension = V.length

fromList :: [Complex Double] -> NodeFeature
fromList = V.fromList

toList :: NodeFeature -> [Complex Double]
toList = V.toList