module MLGraph.EdgePassHom
  ( EdgePassHom(..)
  , identity
  , compose
  , applyMorphism
  , applyIfMember
  , mkMorphism
  ) where

import MLGraph.FeatureSet (NodeFeatureSet(..))
import MLGraph.Feature (NodeFeature)
import qualified Data.Vector as V
import Data.Complex (Complex)
import Numeric.LinearAlgebra (Matrix)
import qualified Numeric.LinearAlgebra as LA

-- A morphism between NodeFeatureSets
data EdgePassHom = EdgePassHom
  { edgeName :: String
  , source :: NodeFeatureSet
  , target :: NodeFeatureSet
  , matrix :: Matrix (Complex Double)  -- now concrete, not IO
  , fiberEdges :: [String]
  }

-- Applies the morphism to a *single* feature vector
applyMorphism :: EdgePassHom -> NodeFeature -> NodeFeature
applyMorphism (EdgePassHom _ _ _ m _) v =
  V.fromList . LA.toList $ m LA.#> LA.fromList (V.toList v)

-- Applies the morphism to all feature vectors in the source set
applyFeatureSet :: EdgePassHom -> [NodeFeature] -> [NodeFeature]
applyFeatureSet hom = map (applyMorphism hom)

-- Create a morphism if dimensions agree
mkMorphism :: NodeFeatureSet -> NodeFeatureSet -> String -> Matrix (Complex Double) -> Maybe EdgePassHom
mkMorphism src tgt n m =
  let dSrc = ambientDimension src
      dTgt = ambientDimension tgt
      label = n
      preimages = []
      (rows, cols) = LA.size m
  in if dTgt == rows && dSrc == cols
       then Just $ EdgePassHom label src tgt m  preimages
       else Nothing

-- Identity morphism for a NodeFeatureSet
identity :: NodeFeatureSet -> EdgePassHom
identity fs =
  let d = ambientDimension fs
      idMat = LA.ident d
      label = nodeName fs
  in EdgePassHom label fs fs idMat []

-- Compose morphisms if types match
compose :: EdgePassHom -> EdgePassHom -> Maybe EdgePassHom
compose (EdgePassHom n0 b1 c m2 f0) (EdgePassHom n1 a b2 m1 f1)
  = if b1 == b2
      then let n2 = "composite"
               f2 = []
           in Just $ EdgePassHom n2 a c (m2 LA.<> m1) f2
    else Nothing


-- Apply morphism only if the signal belongs to the source set
applyIfMember :: EdgePassHom -> NodeFeature -> Maybe NodeFeature
applyIfMember hom v =
  if isMember (source hom) v
    then Just (applyMorphism hom v)
    else Nothing
