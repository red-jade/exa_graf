defmodule Exa.Graf.Traverse do
  @moduledoc """
  Graph 
  Traversal utilities for graphs and forests.

  Provide generalized traversal for:
  - graphs: Depth First Search (DFS), Breadth First Search (BFS) 
  - forest: traversal of Depth First Forest (DFF)

  Define a common visitor callback struct for implementing
  custom traversals over graphs and forests.
  """

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Types
  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Std.Mol

  alias Exa.Graf.Graf

  # --------------------
  # graph/forest visitor
  # --------------------

  @typedoc """
  Function to initialize the traversal state.

  Default implementation is to return `nil`.
  """
  @type init_fun(s) :: (G.graph() -> s)

  @typedoc """
  Function invoked for a tree or vertex traversal.

  There are two types of traversal:
  - DFS of a graph, or traversal of a DFF forest
  - BFS of a graph

  There are four situations where this type of function is invoked:
  - DFS/DFF/BFS before traversal of a new tree; vert argument is root
  - DFS/DFF before (pre) traversal of branch children
  - BFS any node; or DFF leaf node
  - DFS after (post) traversal of children

  Default implementation passes through the state unchanged.
  """
  @type node_fun(s) :: (s, G.graph(), G.vert() -> s)

  @typedoc """
  Function to convert the final traversal state to a result.

  Typically this will be a simple reformatting, 
  such as reversing lists, or discarding completed counters.

  Default implementation is to pass the state through unchanged.
  """
  @type final_fun(s, r) :: (s -> r)

  defmodule DefaultFuns do
    def nil_state(_), do: nil
    def pass_through(state, _, _), do: state
    def pass_through(state), do: state
  end

  defmodule Visitor do
    @moduledoc """
    Callbacks for traversal of a graph or Depth First Forest.

    Functions invoked for all traversals:
    - `init_state`
    - `pre_tree`
    - `final_result`

    Additional functions for DFS graph search:
    - `pre_node`
    - `post_node`

    Additional functions for BFS graph search:
    - `visit_node`

    Additional functions for DFF traversal:
    - branch nodes:
      - `pre_node`
      - `post_node`
    - leaf nodes:
      - `visit_node`

    Default implementations are provided for all callbacks.
    Only define the callbacks you need.
    """

    import DefaultFuns

    defstruct init_state: &nil_state/1,
              # dfs or bfs or dff start of new tree in the forest
              pre_tree: &pass_through/3,
              # dfs or dff branch preorder before children
              pre_node: &pass_through/3,
              # bfs node or dff leaf 
              visit_node: &pass_through/3,
              # dfs or dff branch postorder after children
              post_node: &pass_through/3,
              final_result: &pass_through/1
  end

  @type visitor(s, r) :: %Visitor{
          init_state: init_fun(s),
          pre_tree: node_fun(s),
          pre_node: node_fun(s),
          visit_node: node_fun(s),
          post_node: node_fun(s),
          final_result: final_fun(s, r)
        }

  defguardp is_visitor(v) when is_struct(v, Visitor)

  # ----------------
  # public functions
  # ----------------

  @doc """
  Traverse a graph.

  The search can be:
  - depth-first: children before siblings
  - breadth-first: siblings before children

  Connectivity can be:
  - weak: in and out adjacency
  - strong: only out adjacency

  If the tree is not connected 
  accordingly to the connectivity argument,
  then the traversal will comprise multiple trees.

  The visitor defines callbacks that are invoked 
  at various points during traversal.

  Optionally, a starting vertex may be provided,
  otherwise the vertex is chosen arbitrarily.
  """
  @spec graph(
          G.graph(),
          G.traversality(),
          G.connectivity(),
          visitor(any(), r),
          nil | G.vert()
        ) :: r
        when r: var
  def graph(g, travy, conn, cb, root \\ nil)
      when (is_nil(root) or is_vert(root)) and is_graph(g) and is_trav(travy) and
             is_conn(conn) and is_visitor(cb) do
    {g |> Graf.verts() |> MapSet.new(), cb.init_state.(g)}
    |> do_tree(root, g, adjacency(conn), cb, search(travy))
    |> cb.final_result.()
  end

  @spec do_tree({G.vset(), s}, nil | G.vert(), G.graph(), G.adjacency(), visitor(s, r), fun()) ::
          r
        when s: var, r: var

  defp do_tree({vs, state}, _root, _g, _adjy, _cb, _search) when set_size(vs) == 0, do: state

  defp do_tree({vs, state}, root, g, adjy, cb, search) do
    root = root(root, vs)

    state
    |> cb.pre_tree.(g, root)
    |> search.([root], g, adjy, vs, cb)
    |> do_tree(nil, g, adjy, cb, search)
  end

  defp root(i, vs) when is_vert(i) and is_set_member(vs, i), do: i
  defp root(nil, vs), do: vs |> Enum.take(1) |> hd()

  defp search(:dfs), do: &dfs/6
  defp search(:bfs), do: &bfs/6

  defp adjacency(:weak), do: :in_out
  defp adjacency(:strong), do: :out

  @spec bfs(s, G.verts(), G.graph(), G.adjacency(), G.vset(), visitor(s, any())) :: {G.vset(), s}
        when s: var

  defp bfs(state, [i | front], g, adjy, vs, cb) do
    vs = MapSet.delete(vs, i)
    hop = g |> Graf.reachable(i, adjy, 1) |> MapSet.intersection(vs)
    vs = MapSet.difference(vs, hop)
    state |> cb.visit_node.(g, i) |> bfs(front ++ MapSet.to_list(hop), g, adjy, vs, cb)
  end

  defp bfs(state, [], _g, _adjy, vs, _cb), do: {vs, state}

  @spec dfs(s, G.verts(), G.graph(), G.adjacency(), G.vset(), visitor(s, any())) :: {G.vset(), s}
        when s: var

  defp dfs(state, [i], g, adjy, vs, cb) when is_set_member(vs, i) do
    vs = MapSet.delete(vs, i)
    pre = cb.pre_node.(state, g, i)
    hop = g |> Graf.reachable(i, adjy, 1) |> MapSet.delete(i)

    {vs, state} =
      Enum.reduce(hop, {vs, pre}, fn j, {vs, state} ->
        dfs(state, [j], g, adjy, vs, cb)
      end)

    {vs, cb.post_node.(state, g, i)}
  end

  defp dfs(state, _v, _g, _adjy, vs, _cb), do: {vs, state}

  @doc """
  Depth-first search of a graph to find a cycle.
  """
  @spec cycle?(G.graph(), G.connectivity()) :: bool()
  def cycle?(g, conn) do
    g |> Graf.verts() |> MapSet.new() |> do_cycle(g, conn)
  end

  @spec do_cycle(G.vset(), G.graph(), G.connectivity()) :: bool()
  defp do_cycle(vs, g, conn) do
    case Enum.take(vs, 1) do
      [] -> false
      [i] when conn == :weak -> vs |> cyc_weak(g, nil, i) |> do_cycle(g, conn)
      [i] when conn == :strong -> vs |> cyc_strong(g, i, MapSet.new()) |> do_cycle(g, conn)
    end
  catch
    {:return, :cyclic} -> true
  end

  @spec cyc_strong(G.vset(), G.graph(), G.vert(), G.vset()) :: G.vset()
  defp cyc_strong(vs, g, i, path) do
    vs = MapSet.delete(vs, i)
    ipath = MapSet.put(path, i)

    Enum.reduce(Graf.neighborhood(g, i, :out), vs, fn
      j, _vs when is_set_member(ipath, j) -> throw({:return, :cyclic})
      j, vs -> cyc_strong(vs, g, j, ipath)
    end)
  end

  @spec cyc_weak(G.vset(), G.graph(), nil | {G.adjacency(), G.vert()}, G.vert()) :: G.vset()
  defp cyc_weak(vs, g, from, i) do
    vs = MapSet.delete(vs, i)

    # backtracking means we cannot use the general traversal
    # which rejects all previously visited nodes 
    # with backtracking we need to know the parent link
    # then handle ins and outs separately
    {ins, outs} = g |> Graf.neighborhood(i, :in_out) |> no_backtrack(from)

    vs =
      Enum.reduce(outs, vs, fn
        j, vs when is_set_member(vs, j) -> cyc_weak(vs, g, {:in, i}, j)
        _, _ -> throw({:return, :cyclic})
      end)

    Enum.reduce(ins, vs, fn
      j, vs when is_set_member(vs, j) -> cyc_weak(vs, g, {:out, i}, j)
      _, _ -> throw({:return, :cyclic})
    end)
  end

  # backtracking would be revisiting the previous source node
  # along the same edge as was used to get to the current node
  # backtracking is allowed if there is another edge between the nodes
  # hence there is an undirected cycle between 
  # two nodes linked by two edges of different directions
  defp no_backtrack(neigh, nil), do: neigh
  defp no_backtrack({ins, outs}, {:in, k}), do: {MapSet.delete(ins, k), outs}
  defp no_backtrack({ins, outs}, {:out, k}), do: {ins, MapSet.delete(outs, k)}

  @doc """
  Traverse a Depth First Forest (DFF)
  by applying callbacks from a visitor.

  Generate the DFF using `Exa.Graf.Graf.spanning_forest/3`.

  The optional graph is only used 
  as an argument to visitor callbacks.
  If it is not needed by the visitor, 
  it can be omitted.
  """
  @spec forest(G.forest(), visitor(any(), r), nil | G.graph()) :: r when r: var
  def forest(forest, cb, g \\ nil)
      when (is_nil(g) or is_graph(g)) and is_forest(forest) and is_visitor(cb) do
    forest
    |> Mol.get(:forest)
    |> Enum.reduce(cb.init_state.(g), fn root, state ->
      state |> cb.pre_tree.(g, root) |> dff(root, forest, cb, g)
    end)
    |> cb.final_result.()
  end

  defp dff(state, i, forest, cb, g) do
    case Mol.get(forest, i) do
      [] ->
        cb.visit_node.(state, g, i)

      children ->
        children
        |> Enum.reduce(cb.pre_node.(state, g, i), fn j, state ->
          dff(state, j, forest, cb, g)
        end)
        |> cb.post_node.(g, i)
    end
  end
end
