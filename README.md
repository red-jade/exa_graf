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
with data stored in ETS.

An abstraction for both graphs types,
using a API behaviour and a generic `Exa.Graf.Graf` interface.
`Graf` uses the core `Exa.Dispatch` to send calls
to the specific implementation.

A generic and flexible way to build graphs from
vertices, vertex ranges, edges and adjacency lists.

Graf data allows self-loops and cyclic graphs, but no multi-edges 
(multiple edges between the same pair of vertices).

Simple queries on the graph,
such lists of elements, presence of specific vert/eddge
and classifying vertices.

Conversion between different representations.

Functions to fetch vertex degrees, neighborhoods
and create degree histograms.

Functions for connectedness, connected components
and reachability.

A hash for graphs and isomorphism tests.

Build 1D and 2D histograms from vertex degrees,
and hence generate a hash for a graph.
Use the hash for a simple isomorphism test.

Serialization of _agra_ data to/from files 
using Elixir term format.

Serialization to/from GraphViz DOT format.

Rendering of GraphViz DOT files 
to PNG,SVG images and other formats

### License

EXA source code is released under the MIT license.

EXA code and documentation are:<br>
Copyright (c) 2024 Mike French
