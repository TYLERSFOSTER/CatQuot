module MLGraph.EdgePassHom
  ( EdgePassHom(..)
  , identity
  , compose
  , applyHom
  , applyIfMember
  , mkHom
  ) where

import MLGraph.FeatureSet (NodeFeatureSet(..))
import MLGraph.Feature (NodeFeature)
import qualified Data.Vector as V
import Data.Complex (Complex)
import Numeric.LinearAlgebra (Matrix, (<>))
import qualified Numeric.LinearAlgebra as LA

-- A morphism between NodeFeatureSets
data EdgePassHom = EdgePassHom
  { source :: NodeFeatureSet
  , target :: NodeFeatureSet
  , matrix :: Matrix (Complex Double)  -- now concrete, not IO
  }

-- Applies the morphism to a *single* feature vector
applyHom :: EdgePassHom -> NodeFeature -> NodeFeature
applyHom (EdgePassHom _ _ m) v =
  V.fromList . LA.toList $ m LA.#> LA.fromList (V.toList v)

-- Applies the morphism to all feature vectors in the source set
applyFeatureSet :: EdgePassHom -> [NodeFeature] -> [NodeFeature]
applyFeatureSet hom = map (applyHom hom)

-- Create a morphism if dimensions agree
mkHom :: NodeFeatureSet -> NodeFeatureSet -> Matrix (Complex Double) -> Maybe EdgePassHom
mkHom src tgt m =
  let dSrc = ambientDimension src
      dTgt = ambientDimension tgt
      (rows, cols) = LA.size m
  in if dTgt == rows && dSrc == cols
       then Just $ EdgePassHom src tgt m
       else Nothing

-- Identity morphism for a NodeFeatureSet
identity :: NodeFeatureSet -> EdgePassHom
identity fs =
  let d = ambientDimension fs
      idMat = LA.ident d
  in EdgePassHom fs fs idMat

-- Compose morphisms if types match
compose :: EdgePassHom -> EdgePassHom -> Maybe EdgePassHom
compose (EdgePassHom b1 c m2) (EdgePassHom a b2 m1)
  | b1 == b2 =
      Just $ EdgePassHom a c (m2 LA.<> m1)
  | otherwise = Nothing

-- Apply morphism only if the signal belongs to the source set
applyIfMember :: EdgePassHom -> NodeFeature -> Maybe NodeFeature
applyIfMember hom v =
  if isMember (source hom) v
    then Just (applyHom hom v)
    else Nothing
