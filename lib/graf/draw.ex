defmodule Exa.Graf.Draw do
  @moduledoc """
  Render directed graph to an image using GraphViz DOT format.

  Use various properties of the graph to affect colors and styles.
  """
  require Logger

  use Exa.Graf.Constants

  alias Exa.Types, as: E

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
  @defcols ["darkred", "darkgreen", "darkblue", "darkgoldenrod", "darkviolet"]

  # ----------------
  # public functions
  # ----------------

  @doc """
  Draw a graph.

  Convert a graph to a DOT file and render to an image.
  """
  @spec graph(G.graph(), E.filename(), D.graph_attrs(), D.format()) :: E.filename()
  def graph(g, outdir, attrs \\ %{}, fmt \\ :png)
      when is_graph(g) and is_map(attrs) and
             is_atom(fmt) do
    g |> Graf.to_dot_file(outdir, attrs) |> elem(0) |> DotRender.render_dot(fmt, outdir)
  end

  @doc """
  Draw a graph using colors based on components.

  Convert a graph to a DOT file and render to an image.
  Use colors of nodes and edges to distinguish components.

  Colors are assigned to the sorted list of component ids.
  There should be at least as many colors as components.

  Nodes are given the color of their component (stroke and label).
  Edges within one component are also given the component color.

  The default color is used for edges that span between components 
  (only required for strongly connected components).
  """
  @spec by_components(
          G.graph(),
          G.components(),
          E.filename(),
          [D.dot_color()],
          D.dot_color(),
          D.format()
        ) :: E.filename()
  def by_components(g, comp, outdir, cols \\ @defcols, defcol \\ @defdef, fmt \\ @deffmt)
      when is_graph(g) and is_map(comp) and is_list(cols) and is_atom(fmt) and
             map_size(comp) <= length(cols) do
    # build a colormap
    cmap = comp |> Map.keys() |> Enum.sort() |> Enum.zip(cols) |> Map.new()

    # color nodes in each component
    attrs =
      Enum.reduce(comp, %{}, fn {icomp, iset}, attrs ->
        col = cmap[icomp]
        node = [color: col, fontcolor: col]
        Enum.reduce(iset, attrs, fn i, attrs -> Map.put(attrs, i, node) end)
      end)

    # color each edge according to component embedding
    attrs =
      g
      |> Graf.component_edges(comp)
      |> Enum.reduce(attrs, fn {e, ecomp}, attrs ->
        col =
          case ecomp do
            {icomp, icomp} -> cmap[icomp]
            _ -> defcol
          end

        Map.put(attrs, e, color: col)
      end)

    graph(g, outdir, attrs, fmt)
  end
end
