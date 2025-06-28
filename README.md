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
- Build `QuotTower` subpackage.
  - Build single level-to-level quotienting $\mathbf{Hask}_{tot}$-module
  - Build full tower $\mathbf{Hask}_{tot}$-module
- Write `QuotTower` message-passing $\mathbf{Kl}_{IO}$-module
  - [...]
- Set up backprop-based optimazation $\mathbf{Kl}_{IO}$-module
  - I suspect there's a general template for this. Find it.
- Add more details to this `README.md`
  - Describe central abstraction in `QuotTower` a bit.
  - Add discussion of relationship to k-WL in `README.md`. 


# Features
## Categorical Message Passing
[...]
### Graphs, Diagrams, and Categories
[...]
## Hierarchical Navigable Small *Message Passing*
[...]
### "Graph U-Nets"
[...]

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

### Relationship to Higher-Order Weisfeiler-Leman (*k*-WL) Algorithms
[...]

## Quotient-Based, Hierarchical Category/Graph Searching
[...]
### Search
[...]
### Simplicial Set Builder
[...]