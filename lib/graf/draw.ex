defmodule Exa.Graf.Draw do
  @moduledoc """
  Render directed graph to an image using GraphViz DOT format.

  Use various properties of the graph to affect colors and styles.
  """
  require Logger

  use Exa.Graf.Constants

  alias Exa.Types, as: E

  alias Exa.Std.Mol

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Graf.DotTypes, as: D

  alias Exa.Graf.Graf
  alias Exa.Graf.DotRender

  # ---------
  # constants
  # ---------

  # default output format
  @deffmt :png

  # default color for inter-component edges
  @defdef "gray80"

  # default color sequence for components
  @defcols [
    "darkred",
    "darkgreen",
    "darkblue",
    "darkgoldenrod",
    "darkviolet",
    "firebrick",
    "darkolivegreen",
    "darkcyan",
    "darkorange",
    "darkmagenta",
    "brown",
    "cadetblue",
    "darkslateblue",
    "gold",
    "darkorchid"
  ]

  # ----------------
  # public functions
  # ----------------

  @doc """
  Draw a graph.

  Convert a graph to a DOT file and render to an image.
  """
  @spec graph(G.graph(), E.filename(), D.graph_attrs(), D.format()) ::
          E.filename() | {:error, any()}
  def graph(g, outdir, attrs \\ %{}, fmt \\ :png)
      when is_graph(g) and is_map(attrs) and is_atom(fmt) do
    case Graf.to_dot_file(g, outdir, attrs) do
      {:error, _} = err -> err
      {dot, _text} -> DotRender.render_dot(dot, fmt, outdir)
    end
  end

  @doc """
  Draw a graph using colors based on partitions of the graph. 

  A partition is a map of non-negative integers to 
  disjoint sets of vertices.
  A partition can be Components or Frontiers.

  Convert a graph to a DOT file and render to an image.
  Use colors of nodes and edges to distinguish partitions.

  Colors are assigned to the sorted list of partition keys 
  (component ids or frontier hop).
  There should be at least as many colors as partitions,
  but if not, the colors are cycled.

  Nodes are given the color of their partition (stroke and label).
  Edges within one partition are also given the component color.

  The default color is used for edges that span between partitions 
  (only required for strongly connected components).

  Components are always complete: every vertex is in a partition.
  Frontiers are not necessarily complete:
  there are some vertices not in any value set, 
  they will also get the default color.

  The default attributes may contain global graph values 
  (such as size, layout, node/edge defaults) 
  or non-color properties for specific nodes or edges.
  For example, frontier partition may want to highlight 
  the shape or fill of the source vertex.
  """
  @spec partitions(
          G.graph(),
          G.partition(),
          E.filename(),
          D.graph_attrs(),
          [D.dot_color()],
          D.dot_color(),
          D.format()
        ) :: E.filename()
  def partitions(
        g,
        parts,
        outdir,
        def_attrs \\ %{},
        cols \\ @defcols,
        defcol \\ @defdef,
        fmt \\ @deffmt
      )
      when is_graph(g) and is_map(parts) and is_list(cols) and is_atom(fmt) and
             map_size(parts) <= length(cols) do
    # invert the partition
    {vidx, eidx} = Graf.partition(g, parts)

    # build colormap for partitions
    cmap = parts |> Map.keys() |> Exa.List.zip_cyclic(cols) |> Map.new()

    # note that DOT semantics seems to be that for repeated properties
    # the last attribute value dominates over previous values
    # so prepends does not overwrite existing default attributes

    # color vertices in each partition
    attrs =
      Enum.reduce(vidx, def_attrs, fn
        {i, nil}, attrs ->
          Mol.prepends(attrs, i, color: defcol, fontcolor: defcol)

        {i, ipart}, attrs ->
          col = cmap[ipart]
          Mol.prepends(attrs, i, color: col, fontcolor: col)
      end)

    # color edges in each partition
    attrs =
      Enum.reduce(eidx, attrs, fn
        {e, {ipart, _j_or_nil}}, attrs when not is_nil(ipart) ->
          Mol.prepends(attrs, e, color: cmap[ipart])

        {e, {nil, _j_or_nil}}, attrs ->
          Mol.prepends(attrs, e, color: defcol)
      end)

    graph(g, outdir, attrs, fmt)
  end
end
