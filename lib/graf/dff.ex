defmodule Exa.Graf.Dff do
  @moduledoc """
  Utilities for a spanning Depth First Forest (DFF).
  """
  require Logger

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Graf.Graf
  alias Exa.Graf.Traverse

  alias Exa.Graf.Traverse.DffDump
  alias Exa.Graf.Traverse.DffPre
  alias Exa.Graf.Traverse.DffPost
  alias Exa.Graf.Traverse.ForestPart
  alias Exa.Graf.Traverse.ForestSpan

  @doc """
  Build a spanning forest for the graph.

  A spanning forest is a sequence of rooted trees:
  - weak: arbitrary edge directions
  - strong: edges directed from the root

  The forest includes all vertices, but only a subset of the edges.

  For weak connectivity, edges are traversed in any direction
  and the tree does not have consistent edge directions.
  Any edge that would connect within the same tree is excluded (cycle).
  There is one tree for each weakly connected component. 
  There are no cross edges between trees.

  For strong connectivity, edges are only traversed in the 
  forward direction, and trees are directed from root to leaves. 

  These classes of edges are excluded from the strong forest:
  - reverse: to ancestors within the same tree (cycles)
  - forward: to non-ancestors on other branches within the same tree (dag)
  - reverse cross: from a later tree to an earlier tree

  By construction, there are no _forward cross_ edges
  from earlier trees to later trees.

  Self-loops do not affect the forest,
  because the source of the edge is already in the tree.

  The forest is represented as a map of vertices 
  to a list of their outgoing directed tree edges (MoL).
  Leaf vertices with no outgoing tree edges 
  do not have an entry in the forest (MoL empty default semantics).
  There is a special key `:forest` that 
  contains a list of the roots of trees.

  The spanning forest is created by a depth-first traversal,
  so it is often called a _depth-first forest_ (DFF).

  The initial root vertex for the depth-first search may be specified
  as an argument, otherwise the root is picked arbitrarily from the graph.
  """
  @spec new(G.graph(), G.connectivity(), nil | G.vert()) :: G.forest()
  def new(g, conn, root \\ nil)
      when is_graph(g) and (is_nil(root) or is_vert(root)) and is_conn(conn) do
    Traverse.graph(g, conn, :dfs, %ForestSpan{}, root)
  end

  @doc """
  Print out an indented view of the traversal of a Depth First Forest.
  """
  @spec dump(G.forest()) :: nil
  def dump(dff) when is_forest(dff), do: Traverse.forest(dff, %DffDump{})

  @doc """
  Convert a Depth First Forest to a _partition_ of the graph.

  Each tree is collected into one partition.

  A weak DFF partition will just be the weak connected components.

  A strong DFF partition will not usually correspond 
  to the strong connected components.
  There will be DFF trees that span more than one strong component.

  For example, consider a graph with a single weak component 
  that is a pure (acyclic) tree...

  The weak DFF partition will just be the tree component.

  The strong components will be the set of individual vertices.
  The strong DFF partition will also be the tree component iff:
  - the tree is strongly directed from the root
  - the DFF starts from the tree root

  Otherwise the DFF partition will be a set of subtrees,
  and vertex fragments, which combine to form the original tree.
  """
  @spec to_partition(G.forest()) :: G.partition()
  def to_partition(dff) when is_forest(dff) do
    Traverse.forest(dff, %ForestPart{})
  end

  @doc """
  Convert a forest to a graph containing just the trees.

  Optionally provide a name for the new graph (default `"forest"`).
  """
  @spec to_graph(G.forest()) :: G.graph()
  def to_graph(dff, name \\ "forest") when is_forest(dff) do
    {roots, dff} = Map.pop(dff, :forest)
    g = Graf.build(:adj, name, roots)

    Enum.reduce(dff, g, fn {i, js}, g ->
      Enum.reduce(js, g, fn j, g -> Graf.add(g, {i, j}) end)
    end)
  end

  @doc "Get a pre-order traversal sequence of vertices."
  @spec preorder(G.forest()) :: G.verts()
  def preorder(dff) when is_forest(dff), do: Traverse.forest(dff, %DffPre{})

  @doc "Get a post-order traversal sequence of vertices."
  @spec postorder(G.forest()) :: G.verts()
  def postorder(dff) when is_forest(dff), do: Traverse.forest(dff, %DffPost{})
end
