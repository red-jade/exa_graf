defmodule Exa.Graf.API do
  @moduledoc """
  A behaviour interface for graph implementations.
  """
  require Logger

  use Exa.Constants

  alias Exa.Types, as: E

  alias Exa.Graf.Types, as: G

  @doc """
  Create a new named graph.

  The first argument is the tag for the result graph type.

  The name argument will be sanitized.
  See `Exa.String.sanitize!/1`.
  """
  @callback new(G.gtype(), G.gname()) :: G.graph()

  @doc "Delete the graph."
  @callback delete(G.graph()) :: true

  @doc "Test if a vertex is present in the graph."
  @callback vert?(G.graph(), G.vert()) :: bool()

  @doc "Test if an edge is present in the graph."
  @callback edge?(G.graph(), G.edge()) :: bool()

  @doc "Get the number of vertices."
  @callback nvert(G.graph()) :: E.count()

  @doc "Get the number of edges."
  @callback nedge(G.graph()) :: E.count()

  @doc "Get the vertices of the graph."
  @callback verts(G.graph()) :: G.verts()

  @doc "Get the edges of the graph."
  @callback edges(G.graph()) :: G.edges()

  @doc "Get an arbitrary vertex from the graph."
  @callback some_vert(G.graph()) :: :error | G.vert()

  @doc """
  Add an element to a graph.
  The element can be:
  - vertex
  - vertex range
  - edge
  - chain of edges
  - out adjacency list
  - lists of the above

  It is not an error to add an existing element.
  Adding repeated vertices or edges is idempotent.
  In particular, there will only be at most one edge 
  between any pair of vertices.

  The only common error condition is:
  - unrecognized graph element

  The graph implementation may check for cyclicity
  and generate an error.
  """
  @callback add(G.graph(), G.gelem()) :: G.graph() | {:error, any()}

  @doc """
  Delete an element of a graph.

  The element can be:
  - vertex
  - vertex range
  - edge
  - chain of edges
  - out adjacency list
  - lists of the above

  It is not an error to delete a non-existing element.

  The only error condition is:
  - unrecognized graph element
  """
  @callback delete(G.graph(), G.gelem()) :: G.graph() | {:error, any()}

  @doc """
  Reverse a graph. 

  Maintain all vertices, but reverse the direction of every edge.

  The result is a new graph with `"_transpose"` appended to the name.
  """
  @callback transpose(G.graph()) :: G.graph()

  @doc """
  Get the degree for a vertex, given an adjacency relationship.

  Returns an error if the vertex does not exist.
  """
  @callback degree(G.graph(), G.vert(), G.adjacency()) ::
              n_in_or_out ::
              G.degree()
              | {n_in :: G.degree(), n_out :: G.degree()}
              | {n_in :: G.degree(), self :: 0 | 1, n_out :: G.degree()}
              | {:error, any()}

  @doc """
  Get the neighbors of a vertex, given an adjacency relationship.

  The source vertex will be included if it has a self-loop:
  - `:in` or `:out` self in the set
  - `:in_out` self in both sets
  - `:in_self_out` self in neither set, 
     but present in the separate field

  Returns an error if the vertex does not exist.
  """
  @callback neighborhood(G.graph(), G.vert(), G.adjacency()) ::
              v_in_or_out ::
              G.vset()
              | {v_in :: G.vset(), v_out :: G.vset()}
              | {v_in :: G.vset(), self :: nil | G.vert(), v_out :: G.vset()}
              | {:error, any()}

  @doc """
  Get the connected components of the graph.

  The components may connect pairs of vertices as:
  - weakly connected: 
    undirected connectivity along any edge in any direction
  - strongly connected: 
    cyclic subgraph with directed path in each direction

  Returns an MoL index of components:
  a map of component id to list of distinct vertices in the component.

  The component id is the minimum vertex id in the component.

  The union of all components is the total set of vertices.
  Every vertex is a member of exactly one component.

  Isolated vertices appear as singleton lists,
  indexed by the vertex id.
  """
  @callback components(G.graph(), G.connectivity()) :: G.components()

  @doc """
  Get the condensation graph of the strongly connected components.

  Each strongly connected component is reduced to a single vertex. 
  The vertex is labelled with the minimum vertex id in the component. 
  The remaining vertices and all edges within the component are deleted.
  Edges between components are retained as edges between 
  the contracted component vertices.

  The condensation graph does not have any cycles.

  The result is a new graph with `"_condensation"` appended to the name.
  """
  @callback condensation(G.graph()) :: G.graph()

  @doc """
  Get the set of vertices reachable from the given vertex
  in a certain number of edge steps (which may be `:infinity`).

  The adjacency argument means:
  - `:in` upstream incoming edges, 
     the set _reaching_ to this vertex
  - `:out` downstream outgoing edges,
     the set _reachable_ from this vertex
  - `:in_out` union of upstream and downstream reachability,
  - `:in_self_out` - same as `:in_out` 

  A vertex is always considered reaching/reachable itself,
  even if it does not have a self-loop edge. 
  So the result will always have at least the source vertex.

  Reachability is defined from a single target,
  not as the union of recursive frontiers. 
  For example, the `:in_out` adjacency means two traversals
  from the target, not a recursive bidirectional exploration
  of the weakly connected component.
  """
  @callback reachable(G.graph(), G.vert(), G.adjacency(), G.nhop()) :: G.vset()
end
