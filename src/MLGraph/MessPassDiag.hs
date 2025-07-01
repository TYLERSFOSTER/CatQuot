module MLGraph.MessPassDiag where

import MLGraph.FeatureSet (NodeFeatureSet, nodeName)
import MLGraph.EdgePassHom (EdgePassHom, source, target)

data MessPassGraph = MessPassGraph
  { g0 :: [NodeFeatureSet]    -- objects
  , g1 :: [EdgePassHom]       -- morphisms
  }

-- Extract node names
nodeNames :: MessPassGraph -> [String]
nodeNames = map nodeName . g0

-- Validate that all morphism endpoints live in g0
validateNodes :: MessPassGraph -> Bool
validateNodes (MessPassGraph objs morphs) =
  let names = map nodeName objs
  in all (\m -> nodeName (source m) `elem` names &&
                nodeName (target m) `elem` names) morphs


-- Validate that all strings in a list are names of nodes in graph
validateSubset :: [String] -> MessPassGraph -> Bool
validateSubset nodelist (MessPassGraph objs _) =
  let names = map nodeName objs
  in all (`elem` names) nodelist