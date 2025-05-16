defmodule Exa.Graf.Gio do
  @moduledoc """
  Read and write graphs:
  - GraphViz DOT format
  - Elixir term literal format (ADJ representation)
  """
  require Logger

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Option

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Std.Mol

  alias Exa.Graf.Adj
  alias Exa.Graf.Graf

  alias Exa.Graf.Gio.DotReader
  alias Exa.Graf.Gio.DotWriter, as: DOT

  alias Exa.Graf.Gio.DotTypes, as: D

  @doc """
  Write a graph to file in GraphViz DOT format.

  The graph `gname` is used as the:
  - title of the DOT graph object
  - key for global properties in the graph attribute map
  - basename for the output file

  There are two approaches to representing undirected graphs:
  1. Create a single directed edge for each undirected edge, 
     then only use _weakly connected_ algorithms.
  2. Create a pair of two directed edges for each undirected edge,
     one in each direction.

  Note that for a consistent undirected graph, 
  _all_ edges will be paired, or _all_ will be single.

  There are two options to control rendering of edges, 
  one for pairs and one for singles.
  Use options for the two scenarios of undirected structure 
  to get the desired edge appearance (see below).

  Return the DOT text as `IO.chardata` and the full output filename.

  Use `Exa.Dot.Render.render_dot/3` 
  to convert the DOT file to PNG or SVG.

  ### Options 

  #### Partition

  `:partition` is a partition (Map of Sets) 
   to be used as spatial layers in the diagram

  Each partition is grouped as `same` rank layer.
  Edges from lower to higher partitions are given `constraint=true` 
  to enforce the sequence of layers.
  Other edges default to `constraint=false`
  and are independent of the layering.

  #### Paired Edges

  `:edge_pair` is a `direction` flag for rendering paired bidirectional edges

  An edge pair comprises two directed edges between the same node endpoints.
  For nodes `i` and `j` the edge pair is `{i, j}` and `{j, i}`.

  When the option is set, the pair is drawn as a single edge:
  - `:none` no arrowheads are drawn, the edge appears undirected
  - `:both` both arrowheads are drawn, the edge appears bidirectional
  - `:forward` (default), or any other value, 
    the paired edges are drawn separately using forward arrowheads

  #### Single Edges

  `:edge_single` is a `direction` flag for rendering individual unpaired edges

  The value may be:
  - `:none` no arrowhead is drawn, the edge appears undirected
  - `:forward` (default), or any other value, 
    arrow is drawn in the forward direction

  Note the `:back` direction value is ignored 
  and a default forward arrow is drawn.
  If you want to reverse arrows, 
  render the `transpose` of the graph.
  """
  @spec to_dot_file(G.graph(), E.filename(), D.graph_attrs(), E.options()) ::
          {E.filename(), IO.chardata()}

  def to_dot_file(g, dotdir, gattrs \\ %{}, opts \\ [])
      when is_graph(g) and is_filename(dotdir) and is_options(opts) do
    Exa.File.ensure_dir!(dotdir)
    gname = Graf.name(g)
    filename = Exa.File.join(dotdir, gname, [@filetype_dot])
    parts = Option.get_map(opts, :partition, nil)

    {vidx, eidx} =
      case parts do
        nil -> {nil, nil}
        parts -> Graf.partition_index(g, parts)
      end

    edirs = {
      Option.get_enum(opts, :edge_pair, [:none, :both, :forward], :forward),
      Option.get_enum(opts, :edge_single, [:none, :forward], :forward)
    }

    dot =
      DOT.new_dot(gname)
      |> DOT.globals(gname, gattrs)
      |> dot_nodes(Graf.verts(g), gattrs, parts, vidx)
      |> dot_edges(Graf.edges(g), gattrs, eidx, edirs)
      |> DOT.end_dot()
      |> DOT.to_file(filename)

    {filename, dot}
  end

  defp dot_nodes(dot, verts, gattrs, nil, nil), do: DOT.nodes(dot, verts, gattrs)

  defp dot_nodes(dot, verts, gattrs, parts, vidx) do
    # layers
    dot =
      parts
      |> Enum.sort()
      |> Enum.reduce(dot, fn {_pid, vset}, dot ->
        dot
        |> DOT.open_subgraph()
        |> DOT.rank(:same)
        |> DOT.nodes(vset, gattrs)
        |> DOT.close_subgraph()
      end)

    # remaining nodes
    orphans = Enum.filter(verts, fn i -> is_nil(vidx[i]) end)
    DOT.nodes(dot, orphans, gattrs)
  end

  defp dot_edges(dot, edges, gattrs, nil, eflags) do
    DOT.edges(dot, edges, gattrs, eflags)
  end

  defp dot_edges(dot, edges, gattrs, eidx, eflags) do
    gattrs =
      Enum.reduce(eidx, gattrs, fn {e, {pi, pj}}, gattrs ->
        con = not is_nil(pi) and not is_nil(pj) and pj > pi
        Mol.prepends(gattrs, e, constraint: con)
      end)

    DOT.edges(dot, edges, gattrs, eflags)
  end

  @doc """
  Read a graph from GraphViz DOT file.

  The graph name is read from the internal DOT digraph name,
  not taken from the file basename.
  """
  @spec from_dot_file(G.gtype(), E.filename()) :: {G.graph(), D.graph_attrs()}
  def from_dot_file(tag \\ :adj, filename) when is_gtype(tag) and is_filename(filename) do
    DotReader.from_dot_file(tag, filename)
  end

  @doc "Read a graph from an ADJ file in Elixir literal format."
  @spec from_adj_file(G.gtype(), E.filename()) :: G.graph() | {:error, any()}
  def from_adj_file(tag \\ :adj, filename) when is_gtype(tag) and is_filename(filename) do
    case Adj.from_adj_file(filename) do
      {:error, _} = err -> err
      adj -> Graf.convert(adj, tag)
    end
  end

  @doc """
  Write a graph to an ADJ file in Elixir literal format.

  Return the full output filename.
  """
  @spec to_adj_file(G.graph(), E.filename()) :: E.filename() | {:error, any()}
  def to_adj_file(g, outdir) when is_graph(g) and is_filename(outdir) do
    g |> Graf.convert(:adj) |> Adj.to_adj_file(outdir)
  end
end
