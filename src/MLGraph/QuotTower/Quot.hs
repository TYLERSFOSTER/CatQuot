module MLGraph.QuotTower.Quot where

import MLGraph.MessPassDiag (MessPassGraph, validateSubset)

contractSubgraph :: MessPassGraph -> [String] -> Maybe MessPassGraph
contractSubgraph mpg nodeList =
  if validateSubset nodeList mpg
    then Just mpg  -- Replace with contracting logic
    else Nothing
