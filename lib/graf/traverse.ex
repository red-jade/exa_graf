defmodule Exa.Graf.Traverse do
  @moduledoc """
  Graph traversal utilities for graphs and forests.

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

  alias Exa.Graf.Traverse.Api.Control
  alias Exa.Graf.Traverse.Api.State

  # -----
  # types
  # -----

  # any struct that implements the Traverse.Api.Control protocol

  defguard is_control(c) when is_struct(c)

  # any struct that implements the Traverse.Api.State protocol

  defguard is_state(s) when is_struct(s)

  @typep traversor() :: {Control.t(), State.t()}

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

  If the tree is not connected, 
  in the sense of the connectivity argument,
  then the traversal will comprise multiple components.
  For a Depth First Forest, the components will be pure trees.

  The visitor defines callbacks that are invoked 
  at various points during traversal.

  Note that the `visit_node` callback is invoked for 
  all nodes, both leaf and branch nodes.

  Optionally, a starting vertex may be provided,
  otherwise the vertex is chosen arbitrarily.
  """
  @spec graph(
          G.graph(),
          G.connectivity(),
          G.traversality() | Control.t(),
          State.t(),
          nil | G.vert()
        ) :: any()
  def graph(g, conn, travy, stat, root \\ nil)
      when is_graph(g) and
             is_conn(conn) and
             (is_trav(travy) or is_control(travy)) and
             is_state(stat) and
             (is_nil(root) or is_vert(root)) do
    if not is_nil(root) and not Graf.vert?(g, root) do
      raise ArgumentError, message: "Vertex #{root} not in graph"
    end

    trav =
      cond do
        travy == :dfs -> Exa.Graf.Traverse.Dfs.new()
        travy == :bfs -> Exa.Graf.Traverse.Bfs.new()
        Protocol.assert_impl!(Control, travy) == :ok -> travy
      end

    adjy =
      case conn do
        :weak -> :in_out
        :strong -> :out
      end

    front_fun = fn i -> g |> Graf.reachable(i, adjy, 1) |> MapSet.delete(i) end

    # TODO - need proper final state merging both trav and state
    #        e.g. and prompt halt with trav from ss all distances

    {Control.init_data(trav, g), State.init_state(stat, g)}
    |> do_component(root, g, front_fun)
    |> elem(1)
    |> State.final_result()
  catch
    :throw, {:halt, state} ->
      # prompt return from a search traversal
      State.final_result(state)
  end

  # traverse a connected component
  @spec do_component(traversor(), nil | G.vert(), G.graph(), fun()) :: traversor()
  defp do_component({trav, state}, root, g, front_fun) do
    case Control.select_root(trav, root) do
      {:empty, trav} ->
        {trav, state}

      {root, trav} ->
        {trav, state}
        |> wrap_node_fun(fn s -> State.pre_component(s, g, root) end)
        |> do_traverse(g, front_fun)
        |> do_component(nil, g, front_fun)
    end
  end

  # traverse the next node 
  @spec do_traverse(traversor(), G.graph(), fun()) :: traversor()
  defp do_traverse({trav, state}, g, front_fun) do
    case Control.pop_node(trav) do
      {:empty, trav} ->
        {trav, state}

      {i, trav} ->
        case State.test_node(state, g, i) do
          {:halt, _state} = halt -> throw(halt)
          :cont -> :ok
        end

        # leaf and branch don't really make sense for bfs
        # because they are executed out-of-sequence wrt children

        case Control.push_nodes(trav, front_fun.(i)) do
          {:leaf, trav} ->
            {trav, state}
            |> wrap_node_fun(fn s -> State.visit_leaf(s, g, i) end)
            |> do_traverse(g, front_fun)

          {:branch, trav} ->
            {trav, state}
            |> wrap_node_fun(fn s -> State.pre_branch(s, g, i) end)
            |> do_traverse(g, front_fun)
            |> wrap_node_fun(fn s -> State.post_branch(s, g, i) end)
        end
    end
  end

  # apply a node fun state change and pass through trav
  defp wrap_node_fun({trav, state}, fun), do: {trav, fun.(state)}

  @doc """
  Traverse a Depth First Forest (DFF)
  by applying callbacks from a visitor.

  Generate the DFF using `Exa.Graf.Graf.spanning_forest/3`.

  The optional graph is only used 
  as an argument to visitor callbacks.
  If it is not needed by the visitor, 
  it can be omitted.
  """
  @spec forest(G.forest(), State.t(), nil | G.graph()) :: any()
  def forest(forest, state, g \\ nil)
      when (is_nil(g) or is_graph(g)) and
             is_forest(forest) and is_state(state) do
    roots = Mol.get(forest, :forest)

    State.init_state(state, g)
    |> reduce(roots, fn root, state ->
      state |> State.pre_component(g, root) |> dff(root, forest, g)
    end)
    |> State.final_result()
  end

  defp dff(state, i, forest, g) do
    case Mol.get(forest, i) do
      [] ->
        State.visit_leaf(state, g, i)

      children ->
        state
        |> State.pre_branch(g, i)
        |> reduce(children, fn j, state -> dff(state, j, forest, g) end)
        |> State.post_branch(g, i)
    end
  end

  # reverse args to pipe accumulator state
  defp reduce(acc, enum, fun), do: Enum.reduce(enum, acc, fun)

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
end
