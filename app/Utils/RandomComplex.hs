module Utils.RandomComplex (randomUnitComplex, randomUnitComplexList) where

import System.Random (randomRIO)
import Data.Complex (Complex(..), cis)

-- Generate one random unit complex number
randomUnitComplex :: IO (Complex Double)
randomUnitComplex = do
  theta <- randomRIO (0, 2 * pi)
  return (cis theta)  -- cis θ = cos θ + i sin θ

-- Generate a list of n random unit complex numbers
randomUnitComplexList :: Int -> IO [Complex Double]
randomUnitComplexList n = mapM (const randomUnitComplex) [1..n]
