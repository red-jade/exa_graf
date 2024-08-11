## EXA Graf

𝔼𝕏tr𝔸 𝔼li𝕏ir 𝔸dditions (𝔼𝕏𝔸)

EXA project index: [exa](https://github.com/red-jade/exa)

Data structure for directed graphs.

Module path: `Exa.Graf`

### Features

The _agra_ (A GRAph Research Adventure) 
data structure for directed graphs.

Conversion between different representations,
such as adjacency lists.

Graf format allows cyclic graphs, but no self-loops or multi-edges 
(multiple edges between the same pair of vertices).

Functions to fetch vertex degrees and neighborhoods.

Build 1D and 2D histograms from vertex degrees,
and hence generate a hash for a graph.
Use the hash for simple isomorphism test.

Serialization of _agra_ data to files 
using Elixir term format.

### License

EXA source code is released under the MIT license.

EXA code and documentation are:<br>
Copyright (c) 2024 Mike French
