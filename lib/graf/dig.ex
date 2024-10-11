defmodule Exa.Graf.Dig do
  @moduledoc """
  Utilities for directed graphs using the Erlang _digraph_ library.

  The graph may be cyclic. Repeated edges are not allowed. 
  There is at most one edge between the same ordered pair of vertices.

  The _digraph_ library stores vertex and edges data in ETS.
  which stores state in a separate proceess.
  So the graph object is stateful, it does not need to be
  threaded through all function calls.

  If you do assign variables to each stage of graph operation,
  the variables are not immutable, the graph they refer to
  will mutate over later operations.

  Operations that mutate graphs are:
  - `add/2`
  - `delete/2`
  - `join/3` 1st argument only

  The digraph is declared as `private` so only the creating
  process can access the graph. This is to prevent 
  attempts at parallel graph algorithms that would 
  update the graph concurrently from many different processes.

  The approach to parallel algorithms for Adj and Dig 
  graphs has to be completely different, so no 
  attempt is made to abstract these through the `Graf` API.

  The graph must be destroyed to free resources
  when the client process is finished with the graph,
  but the client will continue execution.
  If the client exits, then the graph resources 
  are automatically freed.
  """
  use Exa.Graf.Constants

  import Exa.Types

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Std.Mos

  # ---------
  # behaviour
  # ---------

  @behaviour Exa.Graf.API

  @impl true
  def new(:dig, gname) when is_gname(gname) do
    {:dig, gname, :digraph.new([:cyclic, :private])}
  end

  @impl true
  def delete({:dig, _, dig}), do: :digraph.delete(dig)

  @impl true
  def nvert({:dig, _, dig}), do: :digraph.no_vertices(dig)

  @impl true
  def nedge({:dig, _, dig}), do: :digraph.no_edges(dig)

  @impl true
  def verts({:dig, _, dig}), do: :digraph.vertices(dig)

  @impl true
  def edges({:dig, _, dig}), do: digraph_edge_list(dig)

  defp digraph_edge_list(dig) do
    Enum.map(:digraph.edges(dig), fn e ->
      {_id, i, j, _label} = :digraph.edge(dig, e)
      {i, j}
    end)
  end

  defp digraph_edge_set(dig) do
    Enum.reduce(:digraph.edges(dig), MapSet.new(), fn e, eset ->
      {_id, i, j, _label} = :digraph.edge(dig, e)
      MapSet.put(eset, {i, j})
    end)
  end

  @impl true
  def some_vert({:dig, _, dig}) do
    # assured that nvert > 0 so there is at least one vertex
    # but unfortunately have to fetch all verts just to get one
    # probably some way to select 1 value from the ets vert table
    dig |> :digraph.vertices() |> hd()
  end

  @impl true
  def vert?({:dig, _, dig}, i), do: do_vert?(dig, i)

  defp do_vert?(dig, i), do: !!:digraph.vertex(dig, i)

  @impl true
  def edge?({:dig, _, dig}, {i, j}), do: do_edge?(dig, i, j)

  defp do_edge?(dig, i, j), do: j in :digraph.out_neighbours(dig, i)

  @impl true
  def transpose({:dig, name, _dig} = g) do
    :dig
    |> new(name <> "_transpose")
    |> add(verts(g))
    |> add(Enum.map(edges(g), fn {i, j} -> {j, i} end))
  end

  @impl true

  def degree({:dig, _, dig}, i, :in) when is_vert(i) do
    if do_vert?(dig, i) do
      :digraph.in_degree(dig, i)
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def degree({:dig, _, dig}, i, :out) when is_vert(i) do
    if do_vert?(dig, i) do
      :digraph.out_degree(dig, i)
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def degree({:dig, _, dig}, i, :in_out) when is_vert(i) do
    if do_vert?(dig, i) do
      {:digraph.in_degree(dig, i), :digraph.out_degree(dig, i)}
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def degree({:dig, _, dig}, i, :in_self_out) when is_vert(i) do
    if do_vert?(dig, i) do
      indeg = :digraph.in_degree(dig, i)
      outs = :digraph.out_neighbours(dig, i)
      outdeg = length(outs)
      if i in outs, do: {indeg - 1, 1, outdeg - 1}, else: {indeg, 0, outdeg}
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  @impl true

  def neighborhood({:dig, _, dig}, i, :in) when is_vert(i) do
    if do_vert?(dig, i) do
      dig |> :digraph.in_neighbours(i) |> MapSet.new()
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def neighborhood({:dig, _, dig}, i, :out) when is_vert(i) do
    if do_vert?(dig, i) do
      dig |> :digraph.out_neighbours(i) |> MapSet.new()
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def neighborhood({:dig, _, dig}, i, :in_out) when is_vert(i) do
    if do_vert?(dig, i) do
      {
        dig |> :digraph.in_neighbours(i) |> MapSet.new(),
        dig |> :digraph.out_neighbours(i) |> MapSet.new()
      }
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def neighborhood({:dig, _, dig}, i, :in_self_out) when is_vert(i) do
    if do_vert?(dig, i) do
      ins = dig |> :digraph.in_neighbours(i) |> MapSet.new()
      outs = dig |> :digraph.out_neighbours(i) |> MapSet.new()

      if MapSet.member?(ins, i) do
        {MapSet.delete(ins, i), i, MapSet.delete(outs, i)}
      else
        {ins, nil, outs}
      end
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  @impl true
  def add({:dig, _, dig} = g, gelem) do
    case do_add(dig, gelem) do
      {:error, _} = err -> err
      _ -> g
    end
  end

  @spec do_add(:digraph.graph(), G.gelem()) :: any()

  defp do_add(_dig, []), do: :ok

  defp do_add(dig, i) when is_vert(i), do: :digraph.add_vertex(dig, i)

  defp do_add(dig, {i, j} = e) when is_vert(i) and is_vert(j) do
    if do_edge?(dig, i, j) do
      # add existing edge not an error
      :ok
    else
      case :digraph.add_edge(dig, i, j) do
        [:"$e" | _eid] ->
          :ok

        {:error, {:bad_vertex, k}} ->
          # add new vertex and retry, maybe twice
          do_add(dig, k)
          do_add(dig, e)

        {:error, {:bad_edge, path}} ->
          {:error, "Cyclic path #{inspect(path, charlists: :as_lists)}"}
      end
    end
  end

  defp do_add(dig, chain) when is_chain(chain) do
    Exa.Tuple.reduce(chain, fn j, i ->
      do_add(dig, {i, j})
      j
    end)
  end

  defp do_add(dig, {src, []}), do: do_add(dig, src)

  defp do_add(dig, {src, dsts}) when is_list(dsts) do
    Enum.each(dsts, fn dst -> do_add(dig, {src, dst}) end)
  end

  defp do_add(dig, r) when is_range(r), do: Enum.each(r, &do_add(dig, &1))

  defp do_add(dig, glist) when is_list(glist) do
    Enum.reduce_while(glist, :ok, fn gel, :ok ->
      case do_add(dig, gel) do
        {:error, _} = err -> {:halt, err}
        _ -> {:cont, :ok}
      end
    end)
  end

  defp do_add(_dig, gel) do
    {:error, "Unrecognized graph element #{inspect(gel)}"}
  end

  @impl true
  def delete({:dig, _, dig} = g, gelem) do
    case do_del(dig, gelem) do
      {:error, _} = err -> err
      _ -> g
    end
  end

  @spec do_del(:digraph.graph(), G.gelem()) :: :ok | {:error, any()}

  defp do_del(dig, i) when is_vert(i) do
    # also deletes all edges incident on the vertex
    :digraph.del_vertex(dig, i)
    :ok
  end

  defp do_del(dig, {i, j}) when is_vert(i) and is_vert(j) do
    Enum.reduce_while(:digraph.out_edges(dig, i), :ok, fn edig, :ok ->
      case :digraph.edge(dig, edig) do
        {_eid, ^i, ^j, _} ->
          :digraph.del_edge(dig, edig)
          {:halt, :ok}

        _ ->
          {:cont, :ok}
      end
    end)
  end

  defp do_del(dig, chain) when is_chain(chain) do
    Exa.Tuple.reduce(chain, fn j, i ->
      do_del(dig, {i, j})
      j
    end)
  end

  defp do_del(dig, {src, dsts}) when is_list(dsts) do
    Enum.each(dsts, fn dst -> do_del(dig, {src, dst}) end)
  end

  defp do_del(dig, r) when is_range(r) do
    Enum.each(r, &do_del(dig, &1))
  end

  defp do_del(dig, glist) when is_list(glist) do
    Enum.reduce_while(glist, :ok, fn gelem, :ok ->
      case do_del(dig, gelem) do
        {:error, _} = err -> {:halt, err}
        _ -> {:cont, :ok}
      end
    end)
  end

  defp do_del(_dig, gel) do
    {:error, "Unrecognized graph element #{gel}"}
  end

  @impl true

  def components({:dig, _, dig}, conn) do
    lol =
      case conn do
        :weak -> :digraph_utils.components(dig)
        :strong -> :digraph_utils.strong_components(dig)
      end

    Enum.reduce(lol, Mos.new(), fn verts, comps ->
      Mos.set(comps, Enum.min(verts), verts)
    end)
  end

  @impl true

  def reachable(g, i, adjy, nhop) when adjy in [:in_out, :in_self_out] do
    # faster way to do this when there is lots of overlap (cyclicity)
    MapSet.union(reachable(g, i, :in, nhop), reachable(g, i, :out, nhop))
  end

  def reachable({:dig, _, dig}, i, :in, :infinity) do
    [i] |> :digraph_utils.reaching(dig) |> MapSet.new()
  end

  def reachable({:dig, _, dig}, i, :out, :infinity) do
    [i] |> :digraph_utils.reachable(dig) |> MapSet.new()
  end

  def reachable({:dig, _, dig}, i, adjy, nhop) when adjy in [:in, :out] and is_count(nhop) do
    MapSet.new() |> do_reach(dig, i, adjy, nhop) |> MapSet.new()
  end

  @spec do_reach(MapSet.t(), :digraph.digraph(), :digraph.vertex(), G.adjacency(), E.count()) ::
          MapSet.t()

  defp do_reach(reach, _dig, i, _adjy, 0), do: MapSet.put(reach, i)

  defp do_reach(reach, dig, i, adjy, n) do
    reach = MapSet.put(reach, i)

    neighs =
      case adjy do
        :in -> dig |> :digraph.in_neighbours(i) |> MapSet.new()
        :out -> dig |> :digraph.out_neighbours(i) |> MapSet.new()
      end

    frontier = MapSet.difference(neighs, reach)

    # if frontier is empty, will immediately pass through current reach
    Enum.reduce(frontier, reach, fn j, reach ->
      do_reach(reach, dig, j, adjy, n - 1)
    end)
  end

  @impl true
  def condensation({:dig, gname, dig}) do
    con = :digraph_utils.condensation(dig)
    # new verts are named using list of all old verts in component!
    # clone the condensed graph, renaming all vertices and edges
    clone = :digraph.new()

    vmap =
      Enum.reduce(:digraph.vertices(con), %{}, fn vlist, vmap ->
        id = Enum.min(vlist)
        :digraph.add_vertex(clone, id)
        Map.put(vmap, vlist, id)
      end)

    Enum.each(:digraph.edges(con), fn eid ->
      {_id, v1, v2, _label} = :digraph.edge(con, eid)
      :digraph.add_edge(clone, Map.fetch!(vmap, v1), Map.fetch!(vmap, v2))
    end)

    {:dig, gname <> "_condensation", clone}
  end

  @impl true
  def equal?({:dig, _, dig1}, {:dig, _, dig2}) do
    :digraph.no_vertices(dig1) == :digraph.no_vertices(dig2) and
      :digraph.no_edges(dig1) == :digraph.no_edges(dig2) and
      MapSet.new(:digraph.vertices(dig1)) == MapSet.new(:digraph.vertices(dig2)) and
      digraph_edge_set(dig1) == digraph_edge_set(dig2)
  end
end
