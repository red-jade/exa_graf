defmodule Exa.Graf.Graf do
  @moduledoc """
  The _Graf_ module has three components:
  - define a behaviour for graph modules
  - abstract interface for graph modules
    that dispatches to the correct implementation module
  - common graph algorithms implemented 
    with the abstract interface

  The current list of graph modules is:
  - Agra: in-memory adjacency lists
  - Dig: Erlang `digraph` ETS database

  Graph data structures use tagged tuples.
  Each graph module implements a common API behaviour.
  The _Graf_ module dispatches to those modules 
  based on the tag in the tuple.
  The mechanism is like a _protocol_ for tagged tuples.
  """
  require Logger

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Types
  alias Exa.Types, as: E

  import Exa.Dispatch, only: [dispatch: 4, dispatch: 3]
  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Std.HistoTypes, as: H
  alias Exa.Std.Histo1D
  alias Exa.Std.Histo2D
  alias Exa.Std.Tidal

  alias Exa.Graf.Agra
  alias Exa.Graf.Dig
  alias Exa.Graf.DotReader
  alias Exa.Graf.DotWriter, as: DOT

  alias Exa.Graf.DotTypes, as: D

  # ---------
  # constants
  # ---------

  @disp %{:agra => Agra, :dig => Dig}

  # ----------------
  # public interface
  # ----------------

  @behaviour Exa.Graf.API

  @impl true
  def new(tag, name, cyc \\ :cyclic) do
    # remove punctuation, convert space to '_', truncate for file system
    gname = Exa.String.sanitize!(name, 200)
    dispatch(@disp, tag, :new, [gname, cyc])
  end

  @impl true
  def delete(g) when is_graph(g), do: dispatch(@disp, g, :delete)

  @impl true
  def vert?(g, i) when is_graph(g) and is_vert(i), do: dispatch(@disp, g, :vert?, [i])

  @impl true
  def edge?(g, e) when is_graph(g) and is_edge(e), do: dispatch(@disp, g, :edge?, [e])

  @impl true
  def nvert(g) when is_graph(g), do: dispatch(@disp, g, :nvert)

  @impl true
  def nedge(g) when is_graph(g), do: dispatch(@disp, g, :nedge)

  @impl true
  def verts(g) when is_graph(g), do: dispatch(@disp, g, :verts)

  @impl true
  def edges(g) when is_graph(g), do: dispatch(@disp, g, :edges)

  @impl true
  def add(g, gelem) when is_graph(g), do: dispatch(@disp, g, :add, [gelem])

  @impl true
  def delete(g, gelem) when is_graph(g), do: dispatch(@disp, g, :delete, [gelem])

  @impl true
  def degree(g, i, adjy) when is_graph(g), do: dispatch(@disp, g, :degree, [i, adjy])

  @impl true
  def neighborhood(g, i, adjy) when is_graph(g), do: dispatch(@disp, g, :neighborhood, [i, adjy])

  # -----------------------
  # generic implementations 
  # not dispatched 
  # -----------------------

  @doc """
  Create a new graph and add a list of graph elements.
  """
  @spec build(G.gtype(), G.gname(), G.glist(), G.cyclicity()) :: G.graph()
  def build(tag, gname, glist, cyc \\ :cyclic) when is_gname(gname) and is_list(glist) do
    tag |> new(gname, cyc) |> add(glist)
  end

  @doc "Get the name of a graph."
  @spec name(G.graph()) :: G.gname()
  def name(g) when is_gtype(elem(g, 0)), do: elem(g, 1)

  @doc """
  Change the name of a graph.

  The name argument will be sanitized.
  See `Exa.String.sanitize!/1`.
  """
  @spec rename(G.graph(), G.gname()) :: G.graph()
  def rename(g, gname) when is_graph(g) and is_gname(gname) do
    # remove punctuation, convert space to '_', truncate for file system
    new_name = Exa.String.sanitize!(gname, 200)
    :erlang.setelement(1, g, new_name)
  end

  @doc """
  Convert a graph to a different format.
  """
  @spec convert(G.graph(), G.gtype()) :: G.graph()

  def convert(g, tag) when tag == elem(g, 0), do: g

  def convert(g, tag) when is_graph(g) and is_gtype(tag) do
    new(tag, name(g)) |> add(edges(g)) |> add(verts_isolated(g))
  end

  @doc "Get the isolated vertices that have no incident edges."
  @spec verts_isolated(G.graph()) :: G.verts()
  def verts_isolated(g) when is_graph(g) do
    # note: could be a lot faster to dispatch this to the
    # implementations by promoting it to be in the API
    Enum.filter(verts(g), fn i -> degree(g, i, :inout) == {i, 0, 0} end)
  end

  @doc """
  Get the vertices as a contiguous range, starting at 1,
  plus a sorted list of additional values after the end of the range.
  """
  @spec verts_rangelist(G.graph()) :: Tidal.range_list()
  def verts_rangelist(g) when is_graph(g) do
    g |> verts() |> Tidal.from_list() |> Tidal.to_range_list()
  end

  @doc """
  Create a 1D histogram of the vertex degrees.

  The kind of degree is determined by the adjacency argument:
  - `:in` in degree
  - `:out` out degree
  - `:inout` total degree (in+out)

  Use `Exa.Std.Histo1D.homogeneous/1` to test for univalent graph
  (a graph where every vertex has the same degree).
  """
  @spec degree_histo1d(G.graph(), G.adjacency()) :: H.histo1d()

  def degree_histo1d(g, adjy) when adjy in [:in, :out] do
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      {^i, deg} = degree(g, i, adjy)
      Histo1D.inc(h, deg)
    end)
  end

  def degree_histo1d(g, :inout) do
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      {^i, indeg, outdeg} = degree(g, i, :inout)
      Histo1D.inc(h, indeg + outdeg)
    end)
  end

  @doc """
  Create a 2D histogram of the in and out vertex degrees.
  """
  @spec degree_histo2d(G.graph()) :: H.histo2d()
  def degree_histo2d(g) do
    Enum.reduce(verts(g), Histo2D.new(), fn i, h ->
      {^i, indeg, outdeg} = degree(g, i, :inout)
      Histo2D.inc(h, {indeg, outdeg})
    end)
  end

  @doc """
  Create a hash of the graph.

  The hash should reasonably discriminate graphs
  by their topology, with a simple and relatively fast algorithm.
  The hash ignores vertex and edge identifiers.

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
  @spec hash(G.graph()) :: G.ghash()
  def hash(g) do
    bin = g |> degree_histo2d() |> Enum.sort() |> :erlang.term_to_binary([:local])
    :crypto.hash(:sha256, bin)
  end

  @doc """
  Test two graphs for exact equality,
  including all vertex and edge identifiers.

  This may be very slow, consider spawning as a task:
  - digraph (dig) is stored in ETS (separate process)
    so `Task` asynch/await may be appropriate
  - agra is in-process, so a spawned task will
    may incur an overhead for copying all graph data 
  """
  @spec equal?(G.graph(), G.graph()) :: bool()
  def equal?(g1, g2) when is_graph(g1) and is_graph(g2) do
    case isomorphic?(g1, g2) do
      false -> false
      :undecided -> verts(g1) == verts(g2) and edges(g1) == edges(g2)
    end
  end

  @doc """
  Test two graphs for isomorphism,
  which means a structural equivalence ignoring all vertex identitiers.
  For isomorphic graphs, there is a relabelling of all vertices
  that will make the graphs exactly equal.

  The result is either `false` or `:undecided`.
  A full isomorphic test is not attempted.

  The test compares:
  - number of vertices
  - number of edges
  - hashes of the graphs

  Hashes are currently based on the sorted 
  2D histogram of vertex in-out degrees.

  This is a relatively quick check to excluded many non-isomorphic pairs.
  It does not do a full equality check.
  """
  @spec isomorphic?(G.graph(), G.graph()) :: false | :undecided
  def isomorphic?(g1, g2) when is_graph(g1) and is_graph(g2) do
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

  # --------
  # file I/O
  # --------

  @doc """
  Write a graph to file in GraphViz DOT format.

  The graph `gname` is used as the title of the DOT graph object,
  as a key for global properties in the graph attribute map,
  and as the basename for the output file.

  Return the DOT text as `IO.chardata` and the full output filename.

  Use `Exa.Dot.Render.render_dot/3` 
  to convert the DOT file to PNG or SVG.
  """
  @spec to_dot_file(G.graph(), E.filename(), D.graph_attrs()) ::
          {E.filename(), IO.chardata()}
  def to_dot_file(g, dotdir, gattrs \\ %{})
      when is_graph(g) and is_filename(dotdir) do
    Exa.File.ensure_dir!(dotdir)
    gname = name(g)
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
  Read a graph from GraphViz DOT file.

  The graph name is read from the internal DOT digraph name,
  not taken from the file basename.
  """
  @spec from_dot_file(G.gtype(), E.filename()) :: {G.graph(), D.graph_attrs()}
  def from_dot_file(tag \\ :agra, filename) when is_gtype(tag) and is_filename(filename) do
    DotReader.from_dot_file(tag, filename)
  end

  @doc "Read an graph from an AGR file in Elixir literal format."
  @spec from_agra_file(G.gtype(), E.filename()) :: G.graph() | {:error, any()}
  def from_agra_file(tag \\ :agra, filename) when is_gtype(tag) and is_filename(filename) do
    case Agra.from_agra_file(filename) do
      {:error, _} = err -> err
      agra -> convert(agra, tag)
    end
  end

  @doc """
  Write a graph to an AGR file in Elixir literal format.

  Return the full output filename.
  """
  @spec to_agra_file(G.graph(), E.filename()) :: E.filename() | {:error, any()}
  def to_agra_file(g, outdir) when is_graph(g) and is_filename(outdir) do
    g |> convert(:agra) |> Agra.to_agra_file(outdir)
  end
end
