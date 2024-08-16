defmodule Exa.Graf.Dig do
  @moduledoc """
  Utilities for directed graphs using the Erlang _digraph_ library.

  The graph may be:
  - _cyclic_ generalized directed graph: allow cycles and self-loops
  - _acyclic_ 'Directed Acyclic Graph' (DAG): no cycles or self-loops

  Repeated edges are not allowed. 
  There is at most one edge between the same ordered pair of vertices.

  The _digraph_ library stores vertex and edges data in ETS.
  Erlang _digraph_ and ETS store state in a separate proceess.
  So the graph object is stateful, it does not need to be
  threaded through all function calls.
  However, it does need to be destroyed to free resources
  if the client process is finished with the graph.
  """
  use Exa.Graf.Constants

  import Exa.Types

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  # ---------
  # behaviour
  # ---------

  @behaviour Exa.Graf.API

  @impl true
  def new(:dig, gname, cyc \\ :cyclic) when is_gname(gname) and is_cyc(cyc) do
    {:dig, gname, :digraph.new([cyc])}
  end

  @impl true
  def delete({:dig, _, dig}), do: :digraph.delete(dig)

  @impl true
  def nvert({:dig, _, dig}), do: :digraph.no_vertices(dig)

  @impl true
  def nedge({:dig, _, dig}), do: :digraph.no_edges(dig)

  @impl true
  def verts({:dig, _, dig}), do: dig |> :digraph.vertices() |> vids()

  @impl true
  def edges({:dig, _, dig}), do: dig |> :digraph.edges() |> eids(dig)

  @impl true
  def vert?({:dig, _, dig}, i), do: do_vert?(dig, i)

  @spec do_vert?(:digraph.graph(), G.vert()) :: bool()
  defp do_vert?(dig, i) do
    # yes, should use !! here, but this is clearer
    case :digraph.vertex(dig, vmake(i)) do
      false -> false
      _ -> true
    end
  end

  @impl true
  def edge?({:dig, _, dig}, e), do: do_edge?(dig, e)

  @spec do_edge?(:digraph.graph(), G.edge()) :: bool()
  defp do_edge?(dig, {i, j}) do
    vmake(j) in :digraph.out_neighbours(dig, vmake(i))
  end

  @impl true

  def degree({:dig, _, dig}, i, :in) when is_vert(i) do
    if do_vert?(dig, i) do
      {i, :digraph.in_degree(dig, vmake(i))}
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def degree({:dig, _, dig}, i, :out) when is_vert(i) do
    if do_vert?(dig, i) do
      {i, :digraph.out_degree(dig, vmake(i))}
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def degree({:dig, _, dig}, i, :inout) when is_vert(i) do
    if do_vert?(dig, i) do
      iv = vmake(i)
      {i, :digraph.in_degree(dig, iv), :digraph.out_degree(dig, iv)}
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  @impl true

  def neighborhood({:dig, _, dig}, i, :in) when is_vert(i) do
    if do_vert?(dig, i) do
      {i, vids(:digraph.in_neighbours(dig, vmake(i)))}
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def neighborhood({:dig, _, dig}, i, :out) when is_vert(i) do
    if do_vert?(dig, i) do
      {i, vids(:digraph.out_neighbours(dig, vmake(i)))}
    else
      {:error, "Missing vertex #{i}"}
    end
  end

  def neighborhood({:dig, _, dig}, i, :inout) when is_vert(i) do
    if do_vert?(dig, i) do
      iv = vmake(i)
      {i, vids(:digraph.in_neighbours(dig, iv)), vids(:digraph.out_neighbours(dig, iv))}
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

  defp do_add(dig, i) when is_vert(i) do
    [~c"$v" | ^i] = :digraph.add_vertex(dig, vmake(i))
  end

  defp do_add(dig, {i, j} = e) when is_vert(i) and is_vert(j) do
    if do_edge?(dig, e) do
      # add existing edge not an error
      :ok
    else
      case :digraph.add_edge(dig, vmake(i), vmake(j)) do
        [:"$e" | _eid] ->
          :ok

        {:error, {:bad_vertex, [~c"$v" | k]}} ->
          # add new vertex and retry, maybe twice
          do_add(dig, k)
          do_add(dig, e)

        {:error, {:bad_edge, path}} ->
          {:error, "Cyclic path #{inspect(vids(path), charlists: :as_lists)}"}
      end
    end
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
    {:error, "Unrecognized graph element #{gel}"}
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
    :digraph.del_vertex(dig, vmake(i))
    :ok
  end

  defp do_del(dig, {i, j}) when is_vert(i) and is_vert(j) do
    iv = vmake(i)
    jv = vmake(j)

    Enum.reduce_while(:digraph.out_edges(dig, iv), :ok, fn edig, :ok ->
      case :digraph.edge(dig, edig) do
        {_eid, ^iv, ^jv, _} ->
          :digraph.del_edge(dig, edig)
          {:halt, :ok}

        _ ->
          {:cont, :ok}
      end
    end)
  end

  defp do_del(dig, {src, dsts}) when is_list(dsts) do
    # assume all dsts are verts
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
  def components({:dig, _, dig}) do
    dig
    |> :digraph_utils.components()
    |> Enum.reduce(%{}, fn vdigs, comps ->
      verts = vids(vdigs)
      Map.put(comps, Enum.min(verts), Enum.sort(verts))
    end)
  end

  @impl true

  def reachable({:dig, _, dig}, i, :in) do
    [vmake(i)] |> :digraph_utils.reaching(dig) |> vids() |> MapSet.new()
  end

  def reachable({:dig, _, dig}, i, :out) do
    [vmake(i)] |> :digraph_utils.reachable(dig) |> vids() |> MapSet.new()
  end

  def reachable(g, i, :inout) do
    # faster way to do this when there is lots of overlap (cyclicity)
    # see the recursive Agra implementation
    MapSet.union(reachable(g, i, :in), reachable(g, i, :out))
  end

  # -----------------
  # private functions
  # -----------------

  # convert digraph edges to dig vertex pairs
  @dialyzer {:no_unused, eids: 2}
  @spec eids([:digraph.edge()], :digraph.graph()) :: G.edges()
  defp eids(es, dig) when is_list(es) do
    Enum.map(es, fn e ->
      {_id, v1, v2, _label} = :digraph.edge(dig, e)
      {vid(v1), vid(v2)}
    end)
  end

  # extract id from a list of vertices
  @dialyzer {:no_unused, vids: 1}
  @spec vids([:digraph.vertex()]) :: G.verts()
  defp vids(vs) when is_list(vs), do: Enum.map(vs, &vid/1)

  # extract the id from a dig vertex
  @spec vid(:digraph.vertex()) :: G.vert()
  defp vid([~c"$v" | i]) when is_vert(i), do: i

  # find an existing edge, when the whole edge record is needed
  @spec efind(:digraph.graph(), G.edge()) :: nil | :digraph.edge()
  def efind(dig, {i, j}) do
    jv = vmake(j)

    Enum.find(:digraph.out_edges(dig, vmake(i)), fn eid ->
      {_eid, _iv, kv, _label} = :digraph.edge(dig, eid)
      kv == jv
    end)
  end

  # create a dig vertex 
  @spec vmake(G.vert()) :: :digraph.vertex()
  defp vmake(i) when is_vert(i), do: [~c"$v" | i]
end
