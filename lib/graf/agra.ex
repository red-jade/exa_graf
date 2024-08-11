defmodule Exa.Graf.Agra do
  @moduledoc """
  A data structure for directed graphs using in-memory adjacency sets.

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
  """
  require Logger

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Std.Mos

  alias Exa.Std.HistoTypes, as: H
  alias Exa.Std.Histo1D
  alias Exa.Std.Histo2D
  alias Exa.Std.Tidal

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G
  alias Exa.Graf.DotTypes, as: D

  alias Exa.Graf.DotWriter, as: DOT

  # --------------
  # file interface
  # --------------

  @doc "Read an agraph from file in Elixir literal format."
  @spec from_agr_file(E.filename()) :: G.agra() | {:error, any}
  def from_agr_file(filename) when is_filename(filename) do
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
  @spec to_agr_file(G.agra(), E.filename()) :: E.filename() | {:error, any()}
  def to_agr_file({:agra, gname, _adjs} = agra, outdir)
      when is_filename(outdir) and is_gname(gname) do
    Exa.File.ensure_dir!(outdir)
    path = Exa.File.join(outdir, gname, @filetype_agr)
    # 1. is there a correct way to get a literal text form of data?
    # 2. could compress by removing inadjs and just write outadjs
    #    then recover inadjs by inverting outadjs on read
    # 3. could use :erlang.term_to_binary for compression
    #    but then not human readable
    text = inspect(agra, charlists: :as_lists, limit: :infinity)
    Exa.File.to_file_text(text, path)
    to_string(path)
  rescue
    err -> {:error, err}
  end

  @doc """
  Write an agraph to GraphViz DOT format.

  Graph attributes will be mapped to 
  all nodes, edges, globals and the whole graph 
  (using the agra graph name as key).

  The output filename will be taken from the graph name.

  Return the full relative path and the DOT text data.
  """
  @spec to_dot_file(G.agra(), E.filename(), D.graph_attrs()) ::
          {E.filename(), IO.chardata()} | {:error, any()}
  def to_dot_file({:agra, gname, _} = agra, outdir, gattrs \\ %{}) when is_filename(outdir) do
    Exa.File.ensure_dir!(outdir)
    path = Exa.File.join(outdir, gname, @filetype_dot)

    text =
      DOT.new_dot(gname)
      |> DOT.globals(gname, gattrs)
      |> DOT.nodes(verts(agra), gattrs)
      |> DOT.edges(edges(agra), gattrs)
      |> DOT.end_dot()
      |> DOT.to_file(path)

    Exa.File.to_file_text(path, text)
    {path, text}
  rescue
    err -> {:error, err}
  end

  # ------------
  # construction
  # ------------

  @doc """
  Create a new named graph with a list of graph elements.

  The name argument will be sanitized.
  See `Exa.String.sanitize!/1`.
  """
  @spec new(G.gname(), [G.gelem()]) :: G.agra()
  def new(name, glist \\ [])

  def new(name, []) when is_gname(name) do
    # remove punctuation, convert space to '_', truncate for file system
    gname = Exa.String.sanitize!(name, 200)
    {:agra, gname, {Mos.new(), Mos.new()}}
  end

  def new(name, glist) when is_gname(name) and is_list(glist) do
    name |> new([]) |> add(glist)
  end

  @doc "Rename a graph."
  @spec rename(G.agra(), G.gname()) :: G.agra()
  def rename({:agra, _, data}, new_name), do: {:agra, new_name, data}

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

  The only error condition is:
  - unrecognized graph element

  There are no tests for cyclicity.
  """
  @spec add(G.agra(), G.gelem()) :: G.agra() | {:error, any()}
  def add({:agra, gname, adjs}, gelem) do
    case do_add(adjs, gelem) do
      {:error, _} = err -> err
      adjs -> {:agra, gname, adjs}
    end
  end

  @spec do_add(G.adjmaps(), G.gelem()) :: G.adjmaps() | {:error, any()}

  defp do_add(_dig, []), do: :ok

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

  # TODO - delete

  # -------
  # queries
  # -------

  @doc "Test if a vertex is present in the graph."
  @spec vert?(G.agra(), G.vert()) :: bool()
  def vert?({:agra, _, {_, outadj}}, i), do: is_map_key(outadj, i)

  @doc "Test if an edge is present in the graph."
  @spec edge?(G.agra(), G.edge()) :: bool()
  def edge?({:agra, _, {_, outadj}}, {src, dst}), do: Mos.member?(outadj, src, dst)

  @doc "Get the number of vertices."
  @spec nvert(G.agra()) :: E.count()
  def nvert({:agra, _, {_, outadj}}), do: map_size(outadj)

  @doc "Get the number of edges."
  @spec nedge(G.agra()) :: E.count()
  def nedge({:agra, _, {_, outadj}}), do: Mos.sizes(outadj)

  @doc "Get the vertices of the graph."
  @spec verts(G.agra()) :: G.verts()
  def verts({:agra, _, {_, outadj}}), do: Map.keys(outadj)

  @doc "Get the edges of the graph."
  @spec edges(G.agra()) :: G.edges()
  def edges({:agra, _, {_, outadj}}) do
    Enum.reduce(outadj, [], fn {src, dset}, es ->
      Enum.reduce(dset, es, fn dst, es -> [{src, dst} | es] end)
    end)
  end

  @doc """
  Get the vertices as a contiguous range, starting at 1,
  plus a sorted list of additional values after the end of the range.

  This can be a useful compression if the vertices 
  are always in a range `1..n`.
  """
  @spec verts_rangelist(G.agra()) :: Tidal.range_list()
  def verts_rangelist(g) when is_agra(g) do
    verts(g) |> Tidal.from_list() |> Tidal.to_range_list()
  end

  # ----------------------------------
  # neighborhood, degree and adjacency
  # ----------------------------------

  @doc """
  Get the degree for a vertex, given an adjacency relationship.

  Returns an error if the vertex does not exist.
  """
  @spec degree(G.agra(), G.vert(), G.adjacency()) ::
          {G.vert(), n_in_or_out :: G.degree()}
          | {G.vert(), n_in :: G.degree(), n_out :: G.degree()}
          | {:error, any()}

  def degree({:agra, _, {inadj, _}}, i, _) when not is_map_key(inadj, i) do
    {:error, "Vertex #{i} does not exist"}
  end

  def degree({:agra, _, {inadj, _}}, i, :in) when is_vert(i) do
    Mos.size(inadj, i)
  end

  def degree({:agra, _, {_, outadj}}, i, :out) when is_vert(i) do
    Mos.size(outadj, i)
  end

  def degree({:agra, _, {inadj, outadj}}, i, :inout) when is_vert(i) do
    Mos.size(inadj, i) + Mos.size(outadj, i)
  end

  @doc """
  Get the neighbors of a vertex, given an adjacency relationship.

  Returns an error if the vertex does not exist.
  """
  @spec neighborhood(G.agra(), G.vert(), G.adjacency()) ::
          {G.vert(), in_or_out :: G.verts()}
          | {G.vert(), v_in :: G.verts(), v_out :: G.verts()}
          | {:error, any()}

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

  @doc """
  Create a 1D histogram of the vertex degrees.

  Use `Histo1D.homogeneous?/1` to test for univalent graph
  (a graph where every vertex has the same degree).

  The kind of degree is determined by the adjacency argument:
  - `:in` in degree
  - `:out` out degree
  - `:inout` total degree (in+out)
  """
  @spec degree_histo1d(G.agra(), G.adjacency()) :: H.histo1d()

  def degree_histo1d({:agra, _, {inadj, _}}, :in) do
    Enum.reduce(inadj, Histo1D.new(), fn {_dst, srcset}, h ->
      Histo1D.inc(h, MapSet.size(srcset))
    end)
  end

  def degree_histo1d({:agra, _, {_, outadj}}, :out) do
    Enum.reduce(outadj, Histo1D.new(), fn {_src, dstset}, h ->
      Histo1D.inc(h, MapSet.size(dstset))
    end)
  end

  def degree_histo1d({:agra, _, {inadj, outadj}} = g, :inout) do
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      Histo1D.inc(h, Mos.size(inadj, i) + Mos.size(outadj, i))
    end)
  end

  @doc """
  Create a 2D histogram of the in and out vertex degrees.
  """
  @spec degree_histo2d(G.agra()) :: H.histo2d()
  def degree_histo2d({:agra, _, {inadj, outadj}} = g) do
    Enum.reduce(verts(g), Histo2D.new(), fn i, h ->
      Histo2D.inc(h, {Mos.size(inadj, i), Mos.size(outadj, i)})
    end)
  end

  # ---------------------------
  # hash, isomorphism, equality
  # ---------------------------

  @doc """
  Create a hash of the graph.

  The hash should reasonably discriminate graphs
  by their topology, with a simple and relatively fast algorithm.

  The hash can be used to reject a graph isomorphism test.
  Graphs with different hashes cannot be isomorphic.
  Graphs with the same hash may be isomorphic, or not,
  they are _undecided._

  The current approach is to generate the 2D histogram,
  serialize to a list, sort, convert term to binary,
  then hash using SHA-256.

  The algorithm is not guaranteed to be stable across Erlang releases,
  or even between different Erlang runtime instances.
  The hash should not be distributed or persisted.
  """
  @spec hash(G.agra()) :: G.ghash()
  def hash(g) do
    bin = g |> degree_histo2d() |> Enum.sort() |> :erlang.term_to_binary([:local])
    :crypto.hash(:sha256, bin)
  end

  @doc "Test two graphs for exact equality."
  @spec equal?(G.agra(), G.agra()) :: bool()
  def equal?(g1, g2) when is_agra(g1) and is_agra(g2) do
    case isomorphic?(g1, g2) do
      false -> false
      :undecided -> elem(g1, 2) == elem(g2, 2)
    end
  end

  @doc """
  Test two graphs for isomorphism.

  The result is either `false` or `:undecided`.

  The test compares number of vertices, number of edges,
  then hashes of the graphs, 
  currently based on the sorted 2D histogram of vertex degrees.

  This is a relatively quick check to excluded many non-isomorphic pairs.
  It does not do a full equality check.
  """
  @spec isomorphic?(G.agra(), G.agra()) :: false | :undecided
  def isomorphic?(g1, g2) when is_agra(g1) and is_agra(g2) do
    if nvert(g1) == nvert(g2) and
         nedge(g1) == nedge(g2) and
         hash(g1) == hash(g2) do
      # don't test for equality here
      # keep equality check separate
      :undecided
    else
      false
    end
  end
end
