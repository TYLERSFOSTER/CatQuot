module Utils.RandomMatrix () where

import Utils.RandomComplex (randomUnitComplex)
import Numeric.LinearAlgebra
import System.Random
import Control.Monad (replicateM)
import Data.Complex

-- Generate a d x d hmatrix matrix of unit complex numbers
randomUnitComplexHMatrix :: Int -> IO (Matrix (Complex Double))
randomUnitComplexHMatrix d = do
  entries <- replicateM (d*d) randomUnitComplex
  return $ (d><d) entries
