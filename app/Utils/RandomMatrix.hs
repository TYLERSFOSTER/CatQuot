module Utils.RandomMatrix where

import Utils.RandomComplex (randomUnitComplex)
import Numeric.LinearAlgebra
import Control.Monad (replicateM)

-- Generate a d x d hmatrix matrix of unit complex numbers
randomUnitComplexHMatrix :: Int -> IO (Matrix (Complex Double))
randomUnitComplexHMatrix d = do
  entries <- replicateM (d*d) randomUnitComplex
  return $ (d><d) entries

-- Multiply matrix by complex vector
multiplyMatrixVector :: Matrix (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double)
multiplyMatrixVector m v = m #> v