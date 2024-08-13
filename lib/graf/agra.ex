defmodule Exa.Graf.Agra do
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
  def new(:agra, gname, _cyc \\ :cyclic) when is_gname(gname) do
    {:agra, gname, {Mos.new(), Mos.new()}}
  end

  @impl true
  def delete(g) when is_agra(g), do: true

  @impl true
  def add({:agra, gname, adjs}, gelem) do
    case do_add(adjs, gelem) do
      {:error, _} = err -> err
      adjs -> {:agra, gname, adjs}
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
  def delete({:agra, gname, adjs}, gelem) do
    case do_del(adjs, gelem) do
      {:error, _} = err -> err
      adjs -> {:agra, gname, adjs}
    end
  end

  @spec do_del(G.adjmaps(), G.gelem()) :: G.adjmaps() | {:error, any()}

  defp do_del(adjs, []), do: adjs

  defp do_del({inadj, outadj}, i) when is_vert(i) do
    {
      inadj |> Map.delete(i) |> Mos.remove_all(i),
      outadj |> Map.delete(i) |> Mos.remove_all(i)
    }
  end

  defp do_del({inadj, outadj}, r) when is_range(r) do
    {
      Enum.reduce(r, inadj, fn i, inadj ->
        inadj |> Map.delete(i) |> Mos.remove_all(i)
      end),
      Enum.reduce(r, outadj, fn i, outadj ->
        outadj |> Map.delete(i) |> Mos.remove_all(i)
      end)
    }
  end

  defp do_del({inadj, outadj}, {src, dst} = e) when is_edge(e) do
    {
      Mos.remove(inadj, dst, src),
      Mos.remove(outadj, src, dst)
    }
  end

  defp do_del(adjs, {src, []}), do: do_del(adjs, src)

  defp do_del({inadj, outadj}, {src, dsts}) when is_list(dsts) do
    {
      Enum.reduce(dsts, inadj, fn dst, inadj ->
        Mos.remove(inadj, dst, src)
      end),
      Enum.reduce(dsts, Mos.adds(outadj, src, dsts), fn dst, outadj ->
        Mos.remove(outadj, src, dst)
      end)
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
  def vert?({:agra, _, {_, outadj}}, i), do: is_map_key(outadj, i)

  @impl true
  def edge?({:agra, _, {_, outadj}}, {src, dst}), do: Mos.member?(outadj, src, dst)

  @impl true
  def nvert({:agra, _, {_, outadj}}), do: map_size(outadj)

  @impl true
  def nedge({:agra, _, {_, outadj}}), do: Mos.sizes(outadj)

  @impl true
  def verts({:agra, _, {_, outadj}}), do: Map.keys(outadj)

  @impl true
  def edges({:agra, _, {_, outadj}}) do
    Enum.reduce(outadj, [], fn {src, dset}, es ->
      Enum.reduce(dset, es, fn dst, es -> [{src, dst} | es] end)
    end)
  end

  @impl true
  def degree({:agra, _, {inadj, _}}, i, _) when not is_map_key(inadj, i) do
    {:error, "Vertex #{i} does not exist"}
  end

  def degree({:agra, _, {inadj, _}}, i, :in) when is_vert(i) do
    {i, Mos.size(inadj, i)}
  end

  def degree({:agra, _, {_, outadj}}, i, :out) when is_vert(i) do
    {i, Mos.size(outadj, i)}
  end

  def degree({:agra, _, {inadj, outadj}}, i, :inout) when is_vert(i) do
    {i, Mos.size(inadj, i), Mos.size(outadj, i)}
  end

  @impl true

  def neighborhood({:agra, _, {inadj, _}}, i, _) when not is_map_key(inadj, i) do
    {:error, "Vertex #{i} does not exist"}
  end

  def neighborhood({:agra, _, {inadj, _}}, i, :in) when is_vert(i) do
    {i, inadj |> Mos.get(i) |> MapSet.to_list()}
  end

  def neighborhood({:agra, _, {_, outadj}}, i, :out) when is_vert(i) do
    {i, outadj |> Mos.get(i) |> MapSet.to_list()}
  end

  def neighborhood({:agra, _, {inadj, outadj}}, i, :inout) when is_vert(i) do
    {
      i,
      inadj |> Mos.get(i) |> MapSet.to_list(),
      outadj |> Mos.get(i) |> MapSet.to_list()
    }
  end

  # --------------
  # agra functions
  # --------------

  @doc "Read an agraph from file in Elixir literal format."
  @spec from_agra_file(E.filename()) :: G.agra() | {:error, any}
  def from_agra_file(filename) when is_filename(filename) do
    Exa.File.ensure_file!(filename)
    Logger.info("Read  AGR file: #{filename}")
    filename |> Code.eval_file() |> elem(0)
  rescue
    err -> {:error, err}
  end

  @doc """
  Write an agraph to file in Elixir literal format.

  Return the full relative path.
  """
  @spec to_agra_file(G.agra(), E.filename()) :: E.filename() | {:error, any()}
  def to_agra_file({:agra, gname, _adjs} = agra, outdir)
      when is_filename(outdir) and is_gname(gname) do
    Exa.File.ensure_dir!(outdir)
    path = Exa.File.join(outdir, gname, @filetype_agr)
    # 1. is there a correct way to get a literal text form of data?
    # 2. could compress by removing inadjs and just write outadjs
    #    then recover inadjs by inverting outadjs on read
    # 3. could use :erlang.term_to_binary for compression
    #    but then not human readable/editable
    # 4. could add :text|:binary arg to choose output format
    text = inspect(agra, charlists: :as_lists, limit: :infinity)
    Exa.File.to_file_text(text, path)
    to_string(path)
  rescue
    err -> {:error, err}
  end
end
