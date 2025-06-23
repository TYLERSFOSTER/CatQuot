module FeatureSet (isOfDimension, generateNodeFeatureSet, NodeFeatureSet(..), nodeFeatureSet) where

import Feature (NodeFeature, fromList, modulus)
import Data.Complex
import qualified Data.Vector as V

-- Membership predicate
isPeriodicOf :: Int -> Signal -> Bool
isPeriodicOf p s =
  let l = Signal.modulus s
  in all (\i -> s V.! i == s V.! ((i + p) `mod` l)) [0 .. l - 1]

-- Finite sampling generator
generatePeriodicSignals :: Int -> Int -> [[Complex Double]] -> [Signal]
generatePeriodicSignals l p patterns =
  [ fromList (take l (cycle pat)) 
  | pat <- patterns, length pat == p
  ]

-- Abstract signal set representation
data SignalSet = SignalSet
  { setModulus  :: Int
  , setMultiplier :: Int
  , setPeriod :: Int
  , generate :: [Signal]
  , isMember :: Signal -> Bool
  }

-- Equality only considers structural fields, not the isMember function
instance Eq SignalSet where
  a == b =
    setModulus a == setModulus b &&
    setMultiplier a == setMultiplier b &&
    setPeriod a == setPeriod b

periodicSet :: Int -> Int -> [[Complex Double]] -> SignalSet
periodicSet l m patterns = 
  let thisModulus     = l
      thisMultiplier  = m
      thisPeriod      = l `div` gcd m l
  in SignalSet
    { setModulus    = thisModulus
    , setMultiplier = thisMultiplier
    , setPeriod     = thisPeriod
    , isMember      = isPeriodicOf thisPeriod
    , generate      = generatePeriodicSignals thisModulus thisPeriod patterns
    }
