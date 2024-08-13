defmodule Exa.Graf.API do
  @moduledoc """
  A behaviour interface for graph implementations.
  """
  require Logger

  use Exa.Constants

  alias Exa.Types, as: E

  alias Exa.Graf.Types, as: G

  @doc """
  Create a new named graph with a list of graph elements.

  The first argument is the tag for the result graph type.

  The cyclicity argument is interpreted by 
  the underlying graph implementation.

  The name argument will be sanitized.
  See `Exa.String.sanitize!/1`.
  """
  @callback new(G.gtype(), G.gname(), G.cyclicity()) :: G.graph()

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

  @doc """
  Add an element to a graph.
  The element can be:
  - vertex
  - vertex range
  - edge
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
  - out adjacency list
  - lists of the above

  It is not an error to delete a non-existing element.

  The only error condition is:
  - unrecognized graph element
  """
  @callback delete(G.graph(), G.gelem()) :: G.graph() | {:error, any()}

  @doc """
  Get the degree for a vertex, given an adjacency relationship.
  Returns an error if the vertex does not exist.
  """
  @callback degree(G.graph(), G.vert(), G.adjacency()) ::
              {G.vert(), n_in_or_out :: G.degree()}
              | {G.vert(), n_in :: G.degree(), n_out :: G.degree()}
              | {:error, any()}

  @doc """
  Get the neighbors of a vertex, given an adjacency relationship.

  Returns an error if the vertex does not exist.
  """
  @callback neighborhood(G.graph(), G.vert(), G.adjacency()) ::
              {G.vert(), in_or_out :: G.verts()}
              | {G.vert(), v_in :: G.verts(), v_out :: G.verts()}
              | {:error, any()}
end
