defmodule Exa.Graf.DotWriter do
  @moduledoc """
  Utilities to write directed graphs in GraphViz DOT format.
  """
  require Logger

  alias Exa.Types, as: E

  import Exa.Color.Types
  alias Exa.Color.Col3b

  alias Exa.Text, as: T

  import Exa.Indent, except: [reduce: 3]
  alias Exa.Indent, as: I

  import Exa.Types

  use Exa.Graf.Constants
  alias Exa.Graf.Types, as: G
  alias Exa.Graf.DotTypes, as: D

  # constants ----------

  # list of characters to escape in labels
  @label_escapes ~c<"'>

  # types ----------

  # map key for graph attributes before aliasing
  # - string: graph/subcluster name
  # - vert: node ID integer
  # - atom: :node, :edge and any other attribute key
  # TODO - needs clarifying, why not gkey() including edges
  @typep id() :: G.gname() | G.vert() | atom()
  defguardp is_id(id) when is_string_nonempty(id) or is_int_pos(id) or is_atom(id)

  # something from which node or edge attributes can be extracted
  # local attribute keywords or global graph attributes
  # graphs_attrs is a map with attr_kw as values
  @typep attrs() :: D.attr_kw() | D.graph_attrs()

  # an alias is a name that will be substituted for nodes in the DOT text
  # alian is something from which an alias can be extracted
  # optional alias String, or local attribute keywords, or global graph attributes
  @typep alian() :: nil | String.t() | attrs()

  # document ----------

  @doc """
  Create a new empty DOT document.

  The argument is a name to be used for the 
  internal graph structure (default: "mydot")
  """
  @spec new_dot(String.t()) :: I.indent()
  def new_dot(name \\ "mydot"), do: indent() |> open_graph(name)

  @doc "Close the document and return the textdata."
  @spec end_dot(I.indent()) :: T.textdata()
  def end_dot(io), do: io |> close_graph() |> to_text()

  @doc "Write DOT text data to file."
  @spec to_file(T.textdata(), E.filename()) :: T.textdata()
  def to_file(text, filename) do
    Exa.File.to_file_text(text, filename)
  end

  @doc """
  Pass through reduce for piping DOT info into output text.
  Just swaps argument order for Enumerable and Indent.
  """
  @spec reduce(I.indent(), Enumerable.t(), (any(), I.indent() -> I.indent())) :: I.indent()
  def reduce(io, xs, fun), do: Enum.reduce(xs, io, fun)

  # graph and subgraph ----------

  @doc "Open a new named graph."
  @spec open_graph(I.indent(), String.t()) :: I.indent()
  def open_graph(io, name), do: io |> txtl(["digraph ", name, " {"]) |> push()

  @doc "Open a named subgraph."
  @spec open_subgraph(I.indent()) :: I.indent()
  def open_subgraph(io, name), do: io |> txtl(["subgraph ", name, " {"]) |> push()

  @doc "Open an anonymous subgraph."
  @spec open_subgraph(I.indent()) :: I.indent()
  def open_subgraph(io), do: io |> chr(?{) |> endl() |> push()

  @doc "Close a graph or subgraph."
  @spec close_graph(I.indent()) :: I.indent()
  def close_graph(io), do: io |> pop() |> chr(?}) |> endl()

  # attributes ----------

  @doc "Set the rankdir graph attribute."
  @spec rankdir(I.indent(), D.rankdir()) :: I.indent()
  def rankdir(io, rankdir), do: attribute(io, :rankdir, rankdir)

  @doc "Set the size graph attribute: width and height in inches."
  @spec size(I.indent(), number(), number()) :: I.indent()
  def size(io, w, h), do: attribute(io, :size, {w, h})

  @doc "Set the fixedsize graph attribute."
  @spec fixedsize(I.indent(), bool()) :: I.indent()
  def fixedsize(io, fixed?), do: attribute(io, :fixedsize, fixed?)

  @doc "Set the fontname attribute."
  @spec fontname(I.indent(), String.t()) :: I.indent()
  def fontname(io, font), do: attribute(io, :fontname, font)

  @doc """
  A single top-level standalone attribute.
  The attribute appears on its own, outside any node or edge.
  """
  @spec attribute(I.indent(), String.t() | atom(), any()) :: I.indent()
  def attribute(io, k, v) do
    io |> newl() |> txt(attr(k, v)) |> chr(?;) |> endl()
  end

  # nodes ------------

  @doc """
  Write top-level properties from graph attribute map.
  Inclulde special node/edge properties
  and regular standalone attributes.

  Properties use the keys `:node` or `:edge`
  and are written in node format.

  Other global attributes are keyed by the graph name
  and are written one per line.
  """
  @spec globals(I.indent(), G.gname(), D.graph_attrs()) :: I.indent()
  def globals(io, gname, gattrs) when is_string(gname) do
    io = io |> global(:node, gattrs) |> global(:edge, gattrs)

    case id_attrs!(gname, gattrs) do
      {_, []} ->
        io

      {^gname, attrs} ->
        Enum.reduce(attrs, io, fn {k, v}, io ->
          attribute(io, k, v)
        end)
    end
  end

  @doc """
  Write common top-level node/edge property attributes.

  Use the key `:node` or `:edge`.
  Top-level properties use a node textual format.
  So even the `:edge` properties use a node format.

  Global properties do not use aliases.
  The attributes must be a keyword list,
  or graph attribute map with keyword list values.

  Only write to output if there are attributes present.
  """
  @spec global(I.indent(), :node | :edge, attrs()) :: I.indent()
  def global(io, id, attrs) when id in [:node, :edge] do
    case id_attrs!(id, attrs) do
      {_, []} -> io
      {key, attrs} -> node(io, key, attrs)
    end
  end

  @doc "Write a node with optional alias or attributes."
  @spec node(I.indent(), id(), alian()) :: I.indent()
  def node(io, id, alian \\ nil) when is_id(id) do
    {i, attrs} = id_attrs!(id, alian)
    io |> newl() |> txt(i) |> attrs(attrs) |> chr(?;) |> endl()
  end

  @doc """
  Write a compact list of nodes, without attributes, on one line.
  The optional graph attributes are only for aliases.
  """
  @spec nodes(I.indent(), [id()], D.graph_attrs()) :: I.indent()
  def nodes(io, ids, gattrs \\ %{})

  def nodes(io, [], _), do: io

  def nodes(io, ids, gattrs) when is_list(ids) do
    io
    |> newl()
    |> reduce(ids, fn id, io ->
      {i, attrs} = id_attrs!(id, gattrs)
      io |> txt(i) |> attrs(attrs) |> txt("; ")
    end)
    |> endl()
  end

  # edges ----------

  @doc """
  Write an edge with optional aliases or attributes.

  The attributes are either:
  - global to provide both node aliases
  - just keywords for edge attributes, without aliases
  """
  @spec edge(I.indent(), id(), id(), attrs()) :: I.indent()
  def edge(io, id, jd, attrs \\ []) when is_id(id) and is_id(jd) do
    {i, _} = id_attrs!(id, attrs)
    {j, _} = id_attrs!(jd, attrs)
    # edge attributes do not have aliases
    eattrs =
      cond do
        is_keyword(attrs) -> attrs
        is_map(attrs) -> Map.get(attrs, {id, jd}, [])
      end

    io |> newl() |> txt([i, " -> ", j]) |> attrs(eattrs) |> chr(?;) |> endl
  end

  @doc """
  Write a compact list of edge pairs, without attributes, all on one line.
  The graph attributes are just to find the node aliases.
  """
  @spec edges(I.indent(), [{id(), id()}], D.graph_attrs()) :: I.indent()
  def edges(io, edges, gattrs \\ %{})

  def edges(io, [], _), do: io

  def edges(io, edges, gattrs) when is_list_nonempty(edges) and is_tuple(hd(edges)) do
    io
    |> newl()
    |> reduce(edges, fn {id, jd}, io ->
      {i, _} = id_attrs!(id, gattrs)
      {j, _} = id_attrs!(jd, gattrs)
      eattrs = Map.get(gattrs, {id, jd}, [])
      io |> txt([i, " -> ", j]) |> attrs(eattrs) |> txt("; ")
    end)
    |> endl()
  end

  @doc """
  Write a chain of edges, without attributes, all on one line.
  The graph attributes supply the node aliases.
  """
  @spec chain(I.indent(), [id(), ...], D.graph_attrs()) :: I.indent()
  def chain(io, [id | ids], gattrs \\ %{}) when ids != [] and is_id(id) do
    {i, _} = id_attrs!(id, gattrs)

    io
    |> newl()
    |> txt(i)
    |> reduce(ids, fn jd, io ->
      {j, _} = id_attrs!(jd, gattrs)
      txt(io, [" -> ", j])
    end)
    |> chr(?;)
    |> endl()
  end

  # -----------------
  # private functions
  # -----------------

  # embedded list of attributes enclosed in square brackets: `[...]`
  @spec attrs(I.indent(), D.attr_kw()) :: I.indent()
  defp attrs(io, []), do: io

  defp attrs(io, [{k, v} | attrs]) do
    txt(io, [?\s, ?[, attr(k, v), Enum.map(attrs, fn {k, v} -> [", ", attr(k, v)] end), ?]])
  end

  # convert attribute to text data, with key equals value 
  @spec attr(String.t() | atom(), any()) :: T.textdata()
  defp attr(:label, v), do: ["label=", esc(v)]
  defp attr(k, v), do: [to_string(k), ?=, val(v)]

  # convert an attribute value to text data
  @spec val(any()) :: T.textdata()

  defp val(col) when is_col3f(col) do
    {r, g, b} = col
    [?", Enum.join([r, g, b], ","), ?"]
  end

  # assume CSS4 colors are subset of dot colors
  defp val({name, _col3b} = col) when is_col3name(col), do: T.term_to_text(name)

  defp val(col) when is_col3b(col), do: [?", Col3b.to_hex(col, :rgb), ?"]

  defp val({x, y}), do: [?", val(x), ?,, val(y), ?"]

  defp val(v), do: T.term_to_text(v)

  # quote and escape a string label
  @spec esc(String.t()) :: T.textdata()
  defp esc(str), do: [?", Exa.String.escape(str, @label_escapes), ?"]

  # validate an id and convert to a string
  @spec id_attrs!(id(), alian()) :: {String.t(), D.attr_kw()}
  defp id_attrs!(id, alian) when is_id(id) do
    {name, _attrs} = id_attrs = alian(id, alian)

    if not Exa.String.valid_name?(name) do
      msg = "Illegal node identifier: '#{name}'"
      Logger.error(msg)
      raise ArgumentError, message: msg
    end

    id_attrs
  end

  # get an alias from an alias/attribute argument
  # return any attributes for rendering without the alias
  @spec alian(id(), alian()) :: {String.t(), D.attr_kw()}

  defp alian(i, nil), do: {to_string(i), []}
  defp alian(_, str) when is_string(str), do: {str, []}
  defp alian(i, map) when is_map(map), do: alian(i, map[i])

  defp alian(i, kw) when is_keyword(kw) do
    case kw[@alias] do
      nil -> {to_string(i), kw}
      al -> {al, Keyword.delete(kw, @alias)}
    end
  end
end
