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
  alias Exa.Types, as: E

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G
  alias Exa.Graf.DotTypes, as: D

  alias Exa.Graf.Agra
  alias Exa.Graf.DotReader
  alias Exa.Graf.DotWriter, as: DOT

  alias Exa.Std.HistoTypes, as: H
  alias Exa.Std.Histo1D
  alias Exa.Std.Histo2D

  # -----
  # types
  # -----

  @typedoc """
  Cyclicity property for the whole graph:
  The boolean argument is for cyclicity:
  - `:cyclic` general directed graph, allow cycles and self-loops
  - `:acyclic` Directed Acyclic Graph (DAG), no cycles or self-loops
  """
  @type cyclicity() :: :cyclic | :acyclic

  # -------------------
  # graph create/delete
  # -------------------

  @doc """
  Create a new graph containing a possibly empty list of graph elements.

  Edge creation forces creation of missing vertices.
  """
  @spec new(G.gname(), [G.gelem()], cyclicity()) :: G.dig()
  def new(name, glist \\ [], cyc \\ :cyclic)

  def new(name, [], cyc) when is_gname(name) and cyc in [:cyclic, :acyclic] do
    # remove punctuation, convert space to '_', truncate for file system
    {:dig, Exa.String.sanitize!(name, 200), :digraph.new([cyc])}
  end

  def new(name, glist, cyc) when is_list(glist) do
    name |> new([], cyc) |> add(glist)
  end

  @doc "Delete the graph."
  @spec delete(G.dig()) :: true
  def delete({:dig, _, dig}), do: :digraph.delete(dig)

  @doc "Rename a graph."
  @spec rename(G.dig(), G.gname()) :: G.dig()
  def rename({:dig, _, data}, new_name), do: {:dig, new_name, data}

  # -------------
  # graph queries
  # -------------

  @doc "The number of vertices in the graph."
  @spec nvert(G.dig()) :: E.count()
  def nvert({:dig, _, dig}), do: :digraph.no_vertices(dig)

  @doc "The number of edges in the graph."
  @spec nedge(G.dig()) :: E.count()
  def nedge({:dig, _, dig}), do: :digraph.no_edges(dig)

  @doc "Get the digraph vertices as a list of ids."
  @spec verts(G.dig()) :: G.verts()
  def verts({:dig, _, dig}), do: dig |> :digraph.vertices() |> vids()

  @doc "Get the digraph edges as a list of directed edges."
  @spec edges(G.dig()) :: G.edges()
  def edges({:dig, _, dig}), do: dig |> :digraph.edges() |> eids(dig)

  @doc "Test if the vertex exists in the graph."
  @spec vert?(G.dig(), G.vert()) :: bool()
  def vert?({:dig, _, dig}, i) when is_vert(i), do: do_vert?(dig, i)

  @spec do_vert?(:digraph.graph(), G.vert()) :: bool()
  defp do_vert?(dig, i) do
    # yes, should use !! here, but this is clearer
    case :digraph.vertex(dig, vmake(i)) do
      false -> false
      _ -> true
    end
  end

  @doc "Test if the edge exists in the graph."
  @spec edge?(G.dig(), G.edge()) :: bool()
  def edge?({:dig, _, dig}, e) when is_edge(e), do: do_edge?(dig, e)

  @spec do_edge?(:digraph.graph(), G.edge()) :: bool()
  defp do_edge?(dig, {i, j}) do
    vmake(j) in :digraph.out_neighbours(dig, vmake(i))
  end

  @doc """
  Get the degree for a vertex, given an adjacency relationship.

  Returns an error if the vertex does not exist.
  """
  @spec degree(G.dig(), G.vert(), G.adjacency()) ::
          {G.vert(), in_or_out :: G.degree()}
          | {G.vert(), n_in :: G.degree(), n_out :: G.degree()}
          | {:error, any()}

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

  @doc """
  Get the neighbors of a vertex, given an adjacency relationship.

  Returns an error if the vertex does not exist.
  """
  @spec neighborhood(G.dig(), G.vert(), G.adjacency()) ::
          {G.vert(), in_or_out :: G.verts()}
          | {G.vert(), v_in :: G.verts(), v_out :: G.verts()}
          | {:error, any()}

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

  # -------------------
  # vertex/edge add/del
  # -------------------

  @doc """
  Add a graph element to the graph.

  When an edge is added to the graph, 
  create any vertices that do not already exist.

  If the graph was created _acyclic_ 
  then self-loops and cyclic paths will force an error.

  If the graph is _cyclic_ then self-loops are allowed.

  Repeated edges are not allowed.
  There can be at most one edge with the same ordered pair of endpoints.

  It is not an error to add an existing element.
  Adding repeated vertices or edges is idempotent.
  In particular, there will only be at most one edge 
  between any pair of vertices.

  The only error conditions are:
  - unrecognized graph element
  - adding self-loop or cyclic path to an acyclic graph
  """
  @spec add(G.dig(), G.gelem()) :: G.dig() | {:error, any()}
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

  @doc """
  Delete an element from the graph.

  It is not an error to delete a non-existent element.

  The only error condition is an unrecognized graph element.
  """
  @spec delete(G.dig(), G.gelem()) :: G.dig() | {:error, any()}
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

    Enum.reduce_while(:digraph.out_edges(dig,iv), :ok, fn edig, :ok ->
      case :digraph.edge(dig, edig) do
        {_eid, ^iv, ^jv, _} -> :digraph.del_edge(dig, edig)
             {:halt, :ok}
        _ -> {:cont, :ok}
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

  # -----------
  # conversions
  # -----------

  @doc """
  Write a digraph to file in GraphViz DOT format.

  The graph `gname` is used as the title of the DOT graph object,
  as a key for global properties in the graph attribute map,
  and as the basename for the output file.

  Return the DOT text as `chardata` and the full output filename.

  Use `Exa.Dot.Render.render_dot/3` 
  to convert the DOT file to PNG or SVG.
  """
  @spec to_dot_file(G.dig(), E.filename(), D.graph_attrs()) ::
          {E.filename(), IO.chardata()}
  def to_dot_file({:dig, gname, _} = g, dotdir, gattrs \\ %{})
      when is_filename(dotdir) and is_string(gname) do
    Exa.File.ensure_dir!(dotdir)
    filename = Exa.File.join(dotdir, gname, [@filetype_dot])

    dot =
      DOT.new_dot(gname)
      |> DOT.globals(gname, gattrs)
      |> DOT.nodes(verts(g), gattrs)
      |> DOT.edges(edges(g), gattrs)
      |> DOT.end_dot()
      |> DOT.to_file(filename)

    {filename, dot}
  end

  @doc """
  Read DOT file into digraph.
  """
  @spec from_dot_file(E.filename()) :: {G.dig(), D.graph_attrs()}
  def from_dot_file(filename) when is_filename(filename) do
    # TODO?
    # use DOT filename or internal graph name?
    # must be the graph name to correlate with gattrs
    # maybe issue warning when they are different
    {_dir, name, _types} = Exa.File.split(filename)
    {agr, gattrs} = DotReader.from_dot_file(filename)
    g = new(name) |> add(Agra.edges(agr)) |> add(Agra.verts(agr))
    {g, gattrs}
  end

  # ----------
  # histograms
  # ----------

  @doc """
  Create a 1D histogram of the vertex degrees.

  The kind of degree is determined by the adjacency argument:
  - `:in` in degree
  - `:out` out degree
  - `:inout` total degree (in+out)
  """
  @spec degree_histo1d(G.dig(), G.adjacency()) :: H.histo1d()

  def degree_histo1d({:dig, _, dig} = g, :in) do
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      Histo1D.inc(h, :digraph.in_degree(dig, vmake(i)))
    end)
  end

  def degree_histo1d({:dig, _, dig} = g, :out) do
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      Histo1D.inc(h, :digraph.out_degree(dig, vmake(i)))
    end)
  end

  def degree_histo1d({:dig, _, dig} = g, :inout) do
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      iv = vmake(i)
      deg = :digraph.in_degree(dig, iv) + :digraph.out_degree(dig, iv)
      Histo1D.inc(h, deg)
    end)
  end

  @doc """
  Create a 2D histogram of the in and out vertex degrees.
  """
  @spec degree_histo2d(G.dig()) :: H.histo2d()
  def degree_histo2d({:dig, _, dig} = g) do
    Enum.reduce(verts(g), Histo2D.new(), fn i, h ->
      iv = vmake(i)
      indeg = :digraph.in_degree(dig, iv)
      outdeg = :digraph.out_degree(dig, iv)
      Histo2D.inc(h, {indeg, outdeg})
    end)
  end

  @doc """
  Create a hash of the graph.

  The hash should reasonably discriminate graphs
  by their topology with a simple and relatively fast algorithm.

  The hash can be used to reject a graph isomorphism test.
  Graphs with different hashes cannot be isomorphic.
  Graphs with the same hash may be isomorphic, or not,
  they are _undecided._

  The current approach is to generate the 2D histogram,
  serialize to a list, sort, convert term to binary,
  then hash using SHA-256.

  The algorithm is not guaranteed to be stable over time
  or between different Erlang runtime instances.
  The hash should not be persisted.
  """
  @spec hash(G.dig()) :: G.ghash()
  def hash(g) when is_dig(g) do
    bin = g |> degree_histo2d() |> Enum.sort() |> :erlang.term_to_binary([:local])
    :crypto.hash(:sha256, bin)
  end

  @doc "Test two graphs for exact equality."
  @spec equal?(G.dig(), G.dig()) :: bool()
  def equal?(g1, g2) when is_dig(g1) and is_dig(g2) do
    case isomorphic?(g1, g2) do
      false ->
        false

      :undecided ->
        g1 |> verts() |> Enum.sort() == g2 |> verts() |> Enum.sort() and
          g1 |> edges() |> Enum.sort() == g2 |> edges() |> Enum.sort()
    end
  end

  @doc """
  Test two graphs for isomorphism.

  The result is either `true`, `false` or `:undecided`.

  The test compares number of vertices, then number of edges,
  then hashes of the graphs 
  (currently based on the sorted 2D histogram of vertex degrees).

  Does not do a full equality check.
  """
  @spec isomorphic?(G.dig(), G.dig()) :: bool() | :undecided
  def isomorphic?(g1, g2) when is_dig(g1) and is_dig(g2) do
    if nvert(g1) == nvert(g2) and nedge(g1) == nedge(g2) and hash(g1) == hash(g2) do
      # don't tets for equality here
      # keep equality check separate
      :undecided
    else
      false
    end
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
