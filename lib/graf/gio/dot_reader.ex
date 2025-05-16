defmodule Exa.Graf.Gio.DotReader do
  @moduledoc """
  Utilities to parse directed graphs from GraphViz DOT format.

  Note: the current version does not handle nested sub-graphs properly.
  """
  require Logger
  import Exa.Types
  alias Exa.Types, as: E

  use Exa.Graf.Constants

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G
  alias Exa.Graf.Gio.DotTypes, as: D

  alias Exa.Graf.Graf

  # -----------
  # local types
  # -----------

  # internal graph data from DOT is just verts and edges
  @typep gdata() :: [G.vert() | G.edge()]

  @doc """
  Read a DOT file.

  The result will be a graph using the 
  internal DOT digraph name as the graph name. 
  The type of the output is determined by the `tag` argument.

  Note that the digraph name is used to key global attributes,
  so if the client renames the graph, the attributes must be rekeyed
  to be consistent.

  Comment lines beginning with `'//'` or `'#'` are ignored.
  """
  # dialyzer does not understand that Exa.File.from... can return error?
  @dialyzer {:nowarn_function, from_dot_file: 2}
  @spec from_dot_file(G.gtype(), E.filename()) :: {G.graph(), D.graph_attrs()} | {:error, any()}
  def from_dot_file(tag \\ :adj, filename) when is_gtype(tag) and is_filename(filename) do
    case Exa.File.from_file_text(filename, comments: ["//", "#"]) do
      text when is_string(text) ->
        {gname, gdata, als, gattrs} = text |> lex([]) |> parse()

        # add the node aliases into the attributes
        new_gattrs =
          Enum.reduce(als, gattrs, fn {str, id}, gattrs ->
            new_attrs = gattrs |> Map.get(id, []) |> Keyword.put(@alias, str)
            Map.put(gattrs, id, new_attrs)
          end)

        {Graf.build(tag, gname, gdata), new_gattrs}

      {:error, _} = err ->
        err
    end
  end

  # ------
  # parser 
  # ------

  @typep tok() ::
           String.t()
           | :digraph
           | :subgraph
           | :open_brace
           | :close_brace
           | :open_square
           | :close_square
           | :semi_colon
           | :equals
           | :comma
           | :arrow
           | :node
           | :edge

  # nested traversal structure of graph and nested subgraphs (clusters)
  @typep gstack() :: [G.gname()]

  # next available integer id 
  # existing name=>id mapping
  # graph attributes has optional key-value map 
  # for each vert or edge or graph or cluster
  @typep context() :: {G.vert(), D.aliases(), gstack(), D.graph_attrs()}

  # ------
  # parser 
  # ------

  # parse a series of tokens to create a graph and metadata
  @spec parse([tok()]) :: {G.gname(), gdata(), D.aliases(), D.graph_attrs()}
  defp parse([:digraph, gname, :open_brace | toks]) do
    # digraph name will be the last entry in the stack
    parse(toks, [], {1, %{}, [gname], %{}})
  end

  @spec parse([tok()], gdata(), context()) ::
          {G.gname(), gdata(), D.aliases(), D.graph_attrs()}

  # node declaration
  defp parse([id, :semi_colon | toks], g, ctx) when is_string(id) do
    {i, i_ctx} = id(id, ctx)
    parse(toks, [i | g], i_ctx)
  end

  # node declaration with attributes
  # TODO - need to add node to subgraph
  defp parse([id, :open_square | toks], g, ctx) when is_string(id) do
    {i, {n, als, gstack, gattrs}} = id(id, ctx)
    {attrs, [:semi_colon | rest]} = attrs(toks)
    new_ctx = {n, als, gstack, Map.put(gattrs, i, attrs)}
    parse(rest, [i | g], new_ctx)
  end

  # single edge declaration, or last of chain, no attributes
  defp parse([a, :arrow, b, :semi_colon | toks], g, ctx) when is_string(a) and is_string(b) do
    {i, a_ctx} = id(a, ctx)
    {j, ab_ctx} = id(b, a_ctx)
    parse(toks, [{i, j} | g], ab_ctx)
  end

  # single edge declaration, or last of chain, with attributes
  defp parse([a, :arrow, b, :open_square | toks], g, ctx) when is_string(a) and is_string(b) do
    {i, a_ctx} = id(a, ctx)
    {j, {n, als, gstack, gattrs}} = id(b, a_ctx)
    {attrs, [:semi_colon | rest]} = attrs(toks)
    new_ctx = {n, als, gstack, Map.put(gattrs, {i, j}, attrs)}
    parse(rest, [{i, j} | g], new_ctx)
  end

  # chained edge definitions ... to be continued
  defp parse([a, :arrow, b | toks], g, ctx) when is_string(a) and is_string(b) do
    {i, a_ctx} = id(a, ctx)
    {j, ab_ctx} = id(b, a_ctx)
    # note b is maintained at the head of the toks
    parse([b | toks], [{i, j} | g], ab_ctx)
  end

  # top-level node declarations, add to gattrs under gname_node key
  defp parse([:node, :open_square | toks], g, {next, als, [gname | _] = gstack, gattrs}) do
    {attrs, [:semi_colon | rest]} = attrs(toks)
    new_gattrs = Map.put(gattrs, gname <> "_node", attrs)
    parse(rest, g, {next, als, gstack, new_gattrs})
  end

  # top-level edge declarations, add to gattrs under gname_edge key
  defp parse([:edge, :open_square | toks], g, {next, als, [gname | _] = gstack, gattrs}) do
    {attrs, [:semi_colon | rest]} = attrs(toks)
    new_gattrs = Map.put(gattrs, gname <> "_edge", attrs)
    parse(rest, g, {next, als, gstack, new_gattrs})
  end

  # top-level attributes, not inside [...] added to the gattrs under gname key
  # TODO - handle comma-separated raw attribte values (color, size)
  defp parse([k, :equals, v, :semi_colon | toks], g, {next, als, [gname | _] = gstack, gattrs}) do
    key = k |> to_string() |> String.to_atom()
    # TODO - parse value v 
    val = to_string(v)
    attrs = gattrs |> Map.get(gname, []) |> Keyword.put(key, val)
    new_gattrs = Map.put(gattrs, gname, attrs)
    parse(toks, g, {next, als, gstack, new_gattrs})
  end

  # open subgraph clusters 
  defp parse([:subgraph, cluster, :open_brace | toks], g, {next, als, gstack, gattrs}) do
    # TODO - handle clusters
    parse(toks, g, {next, als, [to_string(cluster) | gstack], gattrs})
  end

  # anonymous rank grouping, give it generated name
  defp parse([:open_brace | toks], g, {next, als, gstack, gattrs}) do
    parse(toks, g, {next + 1, als, [~s"rank_#{next}" | gstack], gattrs})
  end

  # the final close brace to end the graph definition
  # the digraph name is the remaining entry in the stack
  defp parse([:close_brace], g, {_next, als, [gname], gattrs}) do
    {gname, Enum.reverse(g), als, gattrs}
  end

  # close brace at end the anonymous rank group, or subgroup cluster
  defp parse([:close_brace | toks], g, {next, als, [_ | gstack], gattrs}) do
    parse(toks, g, {next, als, gstack, gattrs})
  end

  # attribute definitions ----------

  # dialyzer does not like is_keyword
  @dialyzer {:nowarn_function, attrs: 2}
  @spec attrs([tok()], D.attr_kw()) :: {D.attr_kw(), [tok()]}
  defp attrs(toks, attrs \\ [])

  defp attrs([k, :equals, v | toks], attrs)
       when is_string(k) and is_string(v) and is_keyword(attrs) do
    key = String.to_atom(k)
    # TODO - parse value v
    attrs(toks, [{key, v} | attrs])
  end

  # this comma is attribute separator, not vector value separator
  defp attrs([:comma | toks], attrs), do: attrs(toks, attrs)
  defp attrs([:close_square | toks], attrs), do: {attrs, toks}

  # get identifier from raw integer or name index
  @spec id(String.t(), context()) :: {G.vert(), context()}

  defp id(<<c, _::binary>> = s, {n, als, gstack, gattrs}) when is_digit(c) do
    # assume identifier starting with digit is an integer
    # parse will fail for bad input, e.g. 12abc
    i = s |> Integer.parse() |> elem(0)

    case Enum.find(als, &(elem(&1, 1) == i)) do
      nil ->
        :ok

      ali ->
        # ints have been generated for string names, now there is literal int
        # TODO - handle mixed integer/alphanum nodes (hope unlikely?)
        msg =
          "DOT graph has mixture of int & string node names. " <>
            "Identifier mapping #{ali} has already been used."

        Logger.error(msg)
        raise ArgumentError, message: msg
    end

    {i, {max(n, i + 1), als, gstack, gattrs}}
  end

  defp id(<<c, _::binary>> = name, {n, als, gstack, gattrs} = ctx) when is_namestart(c) do
    case Map.fetch(als, name) do
      {:ok, id} -> {id, ctx}
      :error -> {n, {n + 1, Map.put(als, name, n), gstack, gattrs}}
    end
  end

  # -----
  # lexer 
  # -----

  @spec lex(String.t(), [tok()]) :: [tok()]

  defp lex(<<ws, rest::binary>>, toks) when is_ws(ws), do: lex(rest, toks)
  defp lex(<<?;, rest::binary>>, toks), do: lex(rest, [:semi_colon | toks])
  defp lex(<<?[, rest::binary>>, toks), do: lex(rest, [:open_square | toks])
  defp lex(<<?], rest::binary>>, toks), do: lex(rest, [:close_square | toks])
  defp lex(<<?=, rest::binary>>, toks), do: lex(rest, [:equals | toks])
  defp lex(<<?,, rest::binary>>, toks), do: lex(rest, [:comma | toks])
  defp lex(<<?-, ?>, rest::binary>>, toks), do: lex(rest, [:arrow | toks])
  defp lex(<<?/, ?*, rest::binary>>, toks), do: lex(comment(rest), toks)

  # opening brace for start of graph declaration, nested cluster and anon rank
  defp lex(<<?{, rest::binary>>, toks), do: lex(rest, [:open_brace | toks])

  # closing brace for end of graph declaration
  defp lex(<<?}, rest::binary>>, toks), do: lex(rest, [:close_brace | toks])

  # fixed keywords
  defp lex("digraph" <> rest, toks), do: lex(rest, [:digraph | toks])
  defp lex("subgraph" <> rest, toks), do: lex(rest, [:subgraph | toks])
  defp lex("node" <> rest, toks), do: lex(rest, [:node | toks])
  defp lex("edge" <> rest, toks), do: lex(rest, [:edge | toks])

  # quoted attr value - or quoted node name?
  defp lex(<<?", rest::binary>>, toks) do
    {str, rest} = quoted(rest, <<>>)
    lex(rest, [str | toks])
  end

  # node name, attr key, or unquoted attr value
  # does allow floating point, hex color, 
  # doesn't handle unquoted comma-separated rgb colors or sizes
  defp lex(<<c, rest::binary>>, toks) when is_alphanum(c) or c == ?# do
    {str, t} = name(rest, <<c>>)
    lex(t, [str | toks])
  end

  defp lex(<<>>, toks), do: Enum.reverse(toks)

  defp lex(s, _toks) do
    msg = "Unrecognized DOT data: #{s}"
    Logger.error(msg)
    raise ArgumentError, message: msg
  end

  # consume a sequence and return a token
  # allow UTF8 in names, quotes and comments

  defp name(<<c, rest::binary>>, name) when is_namechar(c) or c == ?.,
    do: name(rest, <<name::binary, c>>)

  defp name(rest, name), do: {name, rest}

  defp quoted(<<?", rest::binary>>, name), do: {name, rest}
  defp quoted(<<c::utf8, rest::binary>>, name), do: quoted(rest, <<name::binary, c::utf8>>)

  defp comment(<<?*, ?/, rest::binary>>), do: rest
  defp comment(<<_::utf8, rest::binary>>), do: comment(rest)
end
