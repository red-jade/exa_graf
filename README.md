# EXA Graf

𝔼𝕏tr𝔸 𝔼li𝕏ir 𝔸dditions (𝔼𝕏𝔸)

EXA project index: [exa](https://github.com/red-jade/exa)

Data structure and file I/O for directed graphs.

Module path: `Exa.Graf`

## Features

The _adj_ functional data structure for directed graphs,
based on in-memory adjacency lists.

A _dig_ wrapper around the Erlang `digraph` module,
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

Serialization of _adj_ data to/from files 
using Elixir term format.

Serialization to/from GraphViz DOT format.

Rendering of GraphViz DOT files 
to PNG, SVG images and other formats
(if you have GraphViz DOT installed).

## Graph Mutability

The `Exa.Graf.Graf` generic interface supports two graph types:
- `adj`, in-memory in-process functional data structure
- `dig` (Erlang `digraph`) stores data in-memory out-of-process in ETS

Adj is a functional data structure, so every mutation generates
a new version of the graph. Dig is a mutable stateful data structure
maintained in a separate process. 

Mutations to dig graphs will mutate previous references,
so any derived values must be captured before mutation.

For example, consider adding a vertex to a graph:

```
g1 = Graf.new(type, "test")
n1 = Graf.nvert(g1)

g2 = Graf.add(g1,1)
n2 = Graf.nvert(g2)

n = Graf.nvert(g1)
```

If the _type_ is `:adj` the data is functional,
so `n == n1 == 0` and `Graf.equal?(g1,g2) == false`.

If the _type_ is `:dig` the data is mutated, 
so `n == n2 == 1` and `Graf.equal?(g1,g2) == true`.

Operations that mutate graphs are:
  - `add/2`
  - `delete/2`
  - `join/3` 1st argument only
  - `contract_edge/2`
  
Dig graphs should call `delete/1` to delete the graph resources
stored in ETS. Deleting an adj graph is a no-op.

## Building

To bootstrap an `exa_xxx` library build, 
you must run `mix deps.get` twice.

## EXA License

EXA source code is released under the MIT license.

EXA code and documentation are:<br>
Copyright (c) 2024 Mike French
