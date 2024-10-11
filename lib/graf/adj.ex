defmodule Exa.Graf.Adj do
  @moduledoc """
  A data structure for directed graphs using 
  adjacency sets.

  The top-level for a whole graph is a graph record, 
  including the graph name and two adjacency lists (in, out).

  Additions and removals from the graph can be:
  - vertex id (positive integer)
  - id range
  - edge tuple: source and destination ids `{src, dst}`
  - out adjacency list: source and destinations `{src, [dst]}`
  - lists of any of the above

  Self-loops are allowed: 
  an edge with src and dst at the same vertex.

  Multi-edges are not allowed: 
  at most one edge with the same src and dst.

  For example, consider 3 vertices `1,2,3`
  with three edges creating a triangular shape `1->2->3` and `1->3`.
  If the adjacency sets (represented as lists) will be:
  - in : `%{1 => [], 2 => [1], 3 => [1,2]}`
  - out: `%{1 => [2,3], 2 => [3], 3 => []}`

  For very large graphs, you should probably use `Exa.Graf.Dig`,
  which provides a wrapper around the Erlang `:digraph` module,
  because it stores its data in ETS.
  """
  require Logger

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Std.Mos

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  # ---------
  # behaviour
  # ---------

  @behaviour Exa.Graf.API

  @impl true
  def new(:adj, gname) when is_gname(gname) do
    {:adj, gname, {Mos.new(), Mos.new()}}
  end

  @impl true
  def delete(g) when is_adj(g), do: true

  @impl true
  def add({:adj, gname, adjs}, gelem) do
    case do_add(adjs, gelem) do
      {:error, _} = err -> err
      new_adjs -> {:adj, gname, new_adjs}
    end
  end

  @spec do_add(G.adjmaps(), G.gelem()) :: G.adjmaps() | {:error, any()}

  defp do_add(adjs, []), do: adjs

  defp do_add({inadj, outadj}, i) when is_vert(i) do
    {
      Mos.touch(inadj, i),
      Mos.touch(outadj, i)
    }
  end

  defp do_add({inadj, outadj}, r) when is_range(r) do
    {
      Enum.reduce(r, inadj, &Mos.touch(&2, &1)),
      Enum.reduce(r, outadj, &Mos.touch(&2, &1))
    }
  end

  defp do_add({inadj, outadj}, {src, dst} = e) when is_edge(e) do
    {
      inadj |> Mos.touch(src) |> Mos.add(dst, src),
      outadj |> Mos.touch(dst) |> Mos.add(src, dst)
    }
  end

  defp do_add(adjs, chain) when is_chain(chain) do
    1..(tuple_size(chain) - 1)
    |> Enum.reduce({elem(chain, 0), adjs}, fn n, {i, adjs} ->
      j = elem(chain, n)
      {j, do_add(adjs, {i, j})}
    end)
    |> elem(1)
  end

  defp do_add(adjs, {src, []}), do: do_add(adjs, src)

  defp do_add({inadj, outadj}, {src, dsts}) when is_list(dsts) do
    {
      Enum.reduce(dsts, Mos.touch(inadj, src), fn dst, inadj ->
        Mos.add(inadj, dst, src)
      end),
      Enum.reduce(dsts, Mos.adds(outadj, src, dsts), fn dst, outadj ->
        Mos.touch(outadj, dst)
      end)
    }
  end

  defp do_add(adjs, glist) when is_list(glist) do
    Enum.reduce_while(glist, adjs, fn gel, adjs ->
      case do_add(adjs, gel) do
        {:error, _} = err -> {:halt, err}
        new_adjs -> {:cont, new_adjs}
      end
    end)
  end

  defp do_add(_adjs, gel) do
    {:error, "Unrecognized graph element #{gel}"}
  end

  @impl true
  def delete({:adj, gname, adjs}, gelem) do
    case do_del(adjs, gelem) do
      {:error, _} = err -> err
      new_adjs -> {:adj, gname, new_adjs}
    end
  end

  @spec do_del(G.adjmaps(), G.gelem()) :: G.adjmaps() | {:error, any()}

  defp do_del(adjs, []), do: adjs

  defp do_del({inadj, outadj}, i) when is_vert(i) do
    outadj = inadj |> Mos.get(i) |> Enum.reduce(outadj, &Mos.remove(&2, &1, i))
    inadj = outadj |> Mos.get(i) |> Enum.reduce(inadj, &Mos.remove(&2, &1, i))
    {Map.delete(inadj, i), Map.delete(outadj, i)}
  end

  defp do_del(adjs, r) when is_range(r) do
    Enum.reduce(r, adjs, &do_del(&2, &1))
  end

  defp do_del({inadj, outadj}, {src, dst} = e) when is_edge(e) do
    {Mos.remove(inadj, dst, src), Mos.remove(outadj, src, dst)}
  end

  defp do_del(adjs, chain) when is_chain(chain) do
    1..(tuple_size(chain) - 1)
    |> Enum.reduce({elem(chain, 0), adjs}, fn n, {i, adjs} ->
      j = elem(chain, n)
      {j, do_del(adjs, {i, j})}
    end)
    |> elem(1)
  end

  defp do_del(adjs, {_src, []}), do: adjs

  defp do_del({inadj, outadj}, {src, dsts}) when is_list(dsts) do
    {
      Enum.reduce(dsts, inadj, &Mos.remove(&2, &1, src)),
      Mos.removes(outadj, src, dsts)
    }
  end

  defp do_del(adjs, glist) when is_list(glist) do
    Enum.reduce_while(glist, adjs, fn gel, adjs ->
      case do_del(adjs, gel) do
        {:error, _} = err -> {:halt, err}
        new_adjs -> {:cont, new_adjs}
      end
    end)
  end

  defp do_del(_adjs, gel) do
    {:error, "Unrecognized graph element #{gel}"}
  end

  @impl true
  def vert?({:adj, _, {_, outadj}}, i), do: is_map_key(outadj, i)

  @impl true
  def edge?({:adj, _, {_, outadj}}, {src, dst}), do: Mos.member?(outadj, src, dst)

  @impl true
  def nvert({:adj, _, {_, outadj}}), do: map_size(outadj)

  @impl true
  def nedge({:adj, _, {_, outadj}}), do: Mos.sizes(outadj)

  @impl true
  def verts({:adj, _, {_, outadj}}), do: Map.keys(outadj)

  @impl true
  def some_vert({:adj, _, {_, outadj}}) when map_size(outadj) != 0 do
    [{i, _}] = Enum.take(outadj, 1)
    i
  end

  @impl true
  def edges({:adj, _, {_, outadj}}) do
    # each adjmap contains all edges, so just pick one ...
    Enum.reduce(outadj, [], fn {src, dset}, es ->
      Enum.reduce(dset, es, fn dst, es -> [{src, dst} | es] end)
    end)
  end

  @impl true
  def transpose({:adj, name, {inadj, outadj}}) do
    {:adj, name <> "_transpose", {outadj, inadj}}
  end

  @impl true

  def degree({:adj, _, {inadj, _}}, i, _) when not is_map_key(inadj, i) do
    {:error, "Vertex #{i} does not exist"}
  end

  def degree({:adj, _, {inadj, _}}, i, :in) do
    Mos.size(inadj, i)
  end

  def degree({:adj, _, {_, outadj}}, i, :out) do
    Mos.size(outadj, i)
  end

  def degree({:adj, _, {inadj, outadj}}, i, :in_out) do
    {Mos.size(inadj, i), Mos.size(outadj, i)}
  end

  def degree({:adj, _, {inadj, outadj}}, i, :in_self_out) do
    # test for self edge
    if Mos.member?(outadj, i, i) do
      {Mos.size(inadj, i) - 1, 1, Mos.size(outadj, i) - 1}
    else
      {Mos.size(inadj, i), 0, Mos.size(outadj, i)}
    end
  end

  @impl true

  def neighborhood({:adj, _, {inadj, _}}, i, _) when not is_map_key(inadj, i) do
    {:error, "Vertex #{i} does not exist"}
  end

  def neighborhood({:adj, _, {inadj, _}}, i, :in) do
    Mos.get(inadj, i)
  end

  def neighborhood({:adj, _, {_, outadj}}, i, :out) do
    Mos.get(outadj, i)
  end

  def neighborhood({:adj, _, {inadj, outadj}}, i, :in_out) do
    {Mos.get(inadj, i), Mos.get(outadj, i)}
  end

  def neighborhood({:adj, _, {inadj, outadj}}, i, :in_self_out) do
    # test for self edge
    if Mos.member?(outadj, i, i) do
      {
        inadj |> Mos.get(i) |> MapSet.delete(i),
        i,
        outadj |> Mos.get(i) |> MapSet.delete(i)
      }
    else
      {Mos.get(inadj, i), nil, Mos.get(outadj, i)}
    end
  end

  @impl true

  def components({:adj, _, {_, outadj}}, :weak) do
    # union find algorithm
    verts = Map.keys(outadj)

    # build a map of every vert to its component id
    # initially, it will be i => i for every vert
    index = Enum.reduce(verts, %{}, &Map.put(&2, &1, &1))
    comps = Enum.reduce(verts, Mos.new(), &Mos.add(&2, &1, &1))

    # for every edge, merge the components to the lowest comp id
    {_index, comps} =
      Enum.reduce(outadj, {index, comps}, fn {src, dset}, acc ->
        Enum.reduce(dset, acc, fn dst, {index, _} = acc ->
          csrc = Map.fetch!(index, src)
          cdst = Map.fetch!(index, dst)

          cond do
            csrc == cdst -> acc
            csrc < cdst -> merge_comp(acc, csrc, cdst)
            cdst < csrc -> merge_comp(acc, cdst, csrc)
          end
        end)
      end)

    comps
  end

  def components({:adj, _, {inadj, outadj}}, :strong) do
    do_scc(inadj, outadj, inadj |> Map.keys() |> MapSet.new(), %{})
  end

  @spec do_scc(G.adjmap(), G.adjmap(), G.vset(), G.components()) :: G.components()
  defp do_scc(inadj, outadj, vset, comps) do
    # Tarjan is faster (see dfs forest), reachability is easier:
    # in /\ out reachability intersection from any vertex 
    # is guaranteed to find an scc 
    # even if the vertex is isolated and is its own scc
    if MapSet.size(vset) == 0 do
      comps
    else
      [i] = Enum.take(vset, 1)
      ins = do_reach(MapSet.new(), inadj, i, -1)
      outs = do_reach(MapSet.new(), outadj, i, -1)
      comp = MapSet.intersection(ins, outs)
      cid = Exa.Set.min(comp)
      new_comps = Map.put(comps, cid, comp)
      new_vset = MapSet.difference(vset, comp)
      do_scc(inadj, outadj, new_vset, new_comps)
    end
  end

  @impl true

  def reachable({:adj, _, {inadj, _}}, i, :in, :infinity) do
    do_reach(MapSet.new(), inadj, i, -1)
  end

  def reachable({:adj, _, {inadj, _}}, i, :in, nhop) do
    do_reach(MapSet.new(), inadj, i, nhop)
  end

  def reachable({:adj, _, {_, outadj}}, i, :out, :infinity) do
    do_reach(MapSet.new(), outadj, i, -1)
  end

  def reachable({:adj, _, {_, outadj}}, i, :out, nhop) do
    do_reach(MapSet.new(), outadj, i, nhop)
  end

  def reachable(g, i, adjy, nhop) when adjy in [:in_out, :in_self_out] do
    MapSet.union(reachable(g, i, :in, nhop), reachable(g, i, :out, nhop))
  end

  # expand reachability by frontier
  # count down to zero remaining hops (-ve for infinite target)
  # or stop when frontier is empty
  @spec do_reach(MapSet.t(), G.adjmap(), G.vert(), integer()) :: MapSet.t()

  defp do_reach(reach, _adj, i, 0), do: MapSet.put(reach, i)

  defp do_reach(reach, adj, i, n) do
    reach = MapSet.put(reach, i)
    front = adj |> Map.fetch!(i) |> MapSet.difference(reach)
    Enum.reduce(front, reach, fn j, reach -> do_reach(reach, adj, j, n - 1) end)
  end

  @impl true
  def condensation({:adj, gname, gadjs} = g) do
    comps = components(g, :strong)
    index = comp_index(comps)

    {
      :adj,
      gname <> "_condensation",
      Enum.reduce(comps, {%{}, %{}}, fn {cid, vset}, hadjs ->
        hadjs |> do_add(cid) |> condense(gadjs, index, cid, vset)
      end)
    }
  end

  @spec condense(G.adjmaps(), G.adjmaps(), G.component_index(), G.comp_id(), G.vset()) ::
          G.graph()
  defp condense(hadjs, {ginadj, goutadj}, index, icid, vset) do
    Enum.reduce(vset, hadjs, fn i, hadjs ->
      hadjs =
        ginadj
        |> Mos.get(i)
        |> MapSet.difference(vset)
        |> Enum.reduce(MapSet.new(), fn jin, edges ->
          jcid = Map.fetch!(index, jin)
          MapSet.put(edges, {jcid, icid})
        end)
        |> Enum.reduce(hadjs, &do_add(&2, &1))

      hadjs =
        goutadj
        |> Mos.get(i)
        |> MapSet.difference(vset)
        |> Enum.reduce(MapSet.new(), fn jout, edges ->
          jcid = Map.fetch!(index, jout)
          MapSet.put(edges, {icid, jcid})
        end)
        |> Enum.reduce(hadjs, &do_add(&2, &1))

      hadjs
    end)
  end

  @impl true
  def equal?({:adj, _, {_, outadj1}}, {:adj, _, {_, outadj2}}), do: outadj1 == outadj2

  # -------------
  # adj functions
  # -------------

  @doc "Read an adj graph from file in Elixir literal format."
  @spec from_adj_file(E.filename()) :: G.adj() | {:error, any}
  def from_adj_file(filename) when is_filename(filename) do
    Exa.File.ensure_file!(filename)
    Logger.info("Read  ADJ file: #{filename}")
    filename |> Code.eval_file() |> elem(0)
  rescue
    err -> {:error, err}
  end

  @doc """
  Write an adj graph to file in Elixir literal format.

  Return the full relative path.
  """
  @spec to_adj_file(G.adj(), E.filename()) :: E.filename() | {:error, any()}
  def to_adj_file({:adj, gname, _adjs} = adj, outdir)
      when is_filename(outdir) and is_gname(gname) do
    Exa.File.ensure_dir!(outdir)
    path = Exa.File.join(outdir, gname, @filetype_adj)
    # 1. is there a correct way to get a literal text form of data?
    # 2. could compress by removing inadjs and just write outadjs
    #    then recover inadjs by inverting outadjs on read
    # 3. could use :erlang.term_to_binary for compression
    #    but then not human readable/editable
    # 4. could add :text|:binary arg to choose output format
    text = inspect(adj, charlists: :as_lists, limit: :infinity)
    Exa.File.to_file_text(text, path)
    to_string(path)
  rescue
    err -> {:error, err}
  end

  # -----------------
  # private functions
  # -----------------

  @typep index_comp() :: {G.component_index(), G.components()}

  # generate a component index from a component list
  # slightly simplified and optimized version of Mos.invert
  @spec comp_index(G.components()) :: G.component_index()
  defp comp_index(comps) do
    Enum.reduce(comps, %{}, fn {cid, vset}, index ->
      Enum.reduce(vset, index, &Map.put(&2, &1, cid))
    end)
  end

  # merge two components using an index and the component list
  @spec merge_comp(index_comp(), G.vert(), G.vert()) :: index_comp()
  defp merge_comp({index, comps}, cidlo, cidhi) when cidlo < cidhi do
    {
      # for all vertices in the hi comp, 
      # set index to the new lo comp id 
      comps |> Mos.get(cidhi) |> Enum.reduce(index, &Map.put(&2, &1, cidlo)),
      # set the lo comp to union with hi comp, delete hi comp
      Mos.merge(comps, cidlo, cidhi)
    }
  end
end
