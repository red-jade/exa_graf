## EXA Graf

𝔼𝕏tr𝔸 𝔼li𝕏ir 𝔸dditions (𝔼𝕏𝔸)

EXA project index: [exa](https://github.com/red-jade/exa)

Data structure and file I/O for directed graphs.

Module path: `Exa.Graf`

### Features

The _agra_ (A GRAph Research Adventure) 
functional data structure for directed graphs,
based on in-memory adjacency lists.

A wrapper around the Erlang `digraph` module,
with data stored in ETS (in-memory database process).

An abstraction for both graphs types,
using a _behaviour_ API and a generic `Exa.Graf.Graf` interface.
`Graf` uses the core `Exa.Dispatch` to send calls
to the specific implementation.

Conversion between different representations.

A generic and flexible way to build graphs from
vertices, vertex ranges, edges and adjacency lists.

Graf data allows self-loops and cyclic graphs, but no multi-edges 
(multiple edges between the same pair of vertices).

Simple queries on the graph, such as 
lists of elements, presence of specific vert/edge
and vertex classification.

Functions to find:
- vertex degrees
- neighborhoods
- degree histograms.
- connectedness and (weakly) connected components
- reachability sets

Build 1D and 2D histograms from vertex degrees.
Use the 2D in-out adjacency histogram
to create a topology hash for the graph,
and use the hash for a simple isomorphism test.

Relabelling of graphs to permute vertex identifiers.
Combining graphs using _merge_ of vertices and edges,
or _disjoint_ independent addition.

Serialization of _agra_ data to/from files 
using Elixir term format.

Serialization to/from GraphViz DOT format.

Rendering of GraphViz DOT files 
to PNG, SVG images and other formats
(if you have GraphViz DOT installed).

### License

EXA source code is released under the MIT license.

EXA code and documentation are:<br>
Copyright (c) 2024 Mike French
