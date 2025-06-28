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
validateGraph :: MessPassGraph -> Bool
validateGraph (MessPassGraph objs morphs) =
  let names = map nodeName objs
  in all (\m -> nodeName (source m) `elem` names &&
                nodeName (target m) `elem` names) morphs