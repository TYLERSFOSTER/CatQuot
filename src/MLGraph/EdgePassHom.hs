module EdgePassHom
  ( EdgePassHom(..)
  , identity
  , compose
  , applyHom
  , applyIfMember
  , characterHom
  ) where

import FeatureSet (NodeFeatureSet(..))
import Feature (NodeFeature)
import Utils.RandomMatrix (randomUnitComplexHMatrix, multiplyMatrixVector)
import qualified Data.Vector as V
import Data.Complex (Complex(..))
import Numeric.LinearAlgebra

-- Includes a multiplier, which describes how to reindex the signal
data EdgePassHom = EdgePassHom
  { source :: NodeFeatureSet
  , target :: NodeFeatureSet
  , matrix :: IO (Matrix (Complex Double))
  }

-- Applies the morphism to a signal by reindexing
applyHom :: EdgePassHom -> NodeFeature -> NodeFeature
applyHom (EdgePassHom src _ m) f =
  let g = multiplyMatrixVector m f
  in V.generate len $ \n ->
       let i = (m * n) `mod` l
       in s V.! i

-- Identity is the morphism with multiplier 1
identity :: CharacterSet -> CharacterHom
identity cs = CharacterHom cs cs 1

-- Composition multiplies the multipliers
compose :: CharacterHom -> CharacterHom -> Maybe CharacterHom
compose (CharacterHom b1 c m2) (CharacterHom a b2 m1)
  | b1 == b2 && setModulus a == setModulus b1 =
      Just $ CharacterHom a c ((m2 * m1) `mod` setModulus a)
  | otherwise = Nothing

-- Apply morphism only if the signal belongs to the source set
applyIfMember :: Signal -> CharacterHom -> Maybe Signal
applyIfMember s hom =
  if isMember (source hom) s
    then Just (applyHom hom s)
    else Nothing

-- NEW: Construct morphism only if multiplier condition holds: 
-- target.multiplier == source.multiplier * morph.multiplier mod l
characterHom :: CharacterSet -> CharacterSet -> Int -> Maybe CharacterHom
characterHom src tgt m =
  let l = setModulus src
      expected = (setMultiplier src * m) `mod` l
  in if setModulus tgt == l && setMultiplier tgt == expected
       then Just $ CharacterHom src tgt m
       else Nothing
