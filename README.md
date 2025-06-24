<p align="left">
  <picture>
    <source srcset="docs/images/catquot_logo_dark.jpg" media="(prefers-color-scheme: dark)">
    <source srcset="docs/images/catquot_logo_light.jpg" media="(prefers-color-scheme: light)">
    <img src="docs/images/catquot_logo_dark.jpg" alt="tonnetzB" width="400">
  </picture>
</p>

# CatQuot: Graph ML as Categorical ML
Package for HNSW-style searching in Haskell categories using a tower of quotient categories. Inspired by Abdul Malik's PhD thesis.

# TODOs
- Add a version of `NodeFeature` generator that takes feature as input instead of random initilaization
- Finish `EdgePassHom.hs`, currently at complex matrix multiplication
- Build message passing modules:
  - `MessPassDiagram.hs`just the category diagram (no composition) as graph
  - `MessPassCat.hs` impose categorical structure
  - `MessPassGraph.hs` introduces an underlying, guiding graph object
- Add `QuotTower` subpackage
  - Add single level-to-level quotienting module

# Features
## Hierarchical Navigable Small *Message Passing*
### "Graph U-Nets"

<p align="center">
  <picture>
    <source srcset="docs/images/graph_unet_dark.jpg" media="(prefers-color-scheme: dark)">
    <source srcset="docs/images/graph_unet_light.jpg" media="(prefers-color-scheme: light)">
    <img src="docs/images/graph_unet_dark.jpg" alt="tonnetzB" width="700">
  </picture>
</p>
<p align="center" style="font-size: 80%;">
  "Graph U-Net"
</p>

## Categorical Message Passing
## Quotient-Based, Hierarchical Category/Graph Searching
### Search
### Simplicial Set Builder