defmodule Exa.Graf.DepReader do
  @moduledoc """
  Utilities to parse the text output of `mix deps.tree`.
  """
  require Logger
  use Exa.Graf.Constants

  import Exa.Types
  alias Exa.Types, as: E

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G
  alias Exa.Graf.DotTypes, as: D

  alias Exa.Graf.Graf

  @doc """
  Read a deps.tree TXT file.
  """
  @spec from_dep_file(E.filename()) :: {G.graph(), D.graph_attrs()} | {:error, any()}
  # {G.graph(), D.graph_attrs()} | {:error, any()}
  def from_dep_file(tag \\ :agra, filename) when is_gtype(tag) and is_filename(filename) do
    case Exa.File.from_file_lines(filename) do
      lines when is_list(lines) ->
        {_, basename, _} = Exa.File.split(filename)
        g = Graf.new(:agra, basename)

        {index, g, labels} =
          lines
          |> Enum.map(&lex(&1, 0))
          |> parse([], %{}, 0, g, %{})

        # global graph attributes
        gattrs = Map.put(%{}, basename, rankdir: :BT)

        # invert the name index to get graph attribute alias map
        gattrs =
          Enum.reduce(index, gattrs, fn {name, i}, gattrs ->
            label = Map.fetch!(labels, i)
            vcol = if exa?(name), do: "darkred", else: "gray"
            Map.put(gattrs, i, alias: name, label: label, color: vcol)
          end)

        # edge attributes
        gattrs =
          Enum.reduce(Graf.edges(g), gattrs, fn {i, j} = e, gattrs ->
            iexa? = exa?(gattrs, i)
            jexa? = exa?(gattrs, j)

            ecol =
              cond do
                iexa? and jexa? -> "firebrick2"
                iexa? -> "firebrick3"
                true -> "gray"
              end

            Map.put(gattrs, e, color: ecol)
          end)

        {g, gattrs}

      {:error, _} = err ->
        err
    end
  end

  defp exa?(gattrs, i) do
    gattrs |> Map.fetch!(i) |> Keyword.get(:alias) |> exa?()
  end

  defp exa?(name), do: String.starts_with?(name, "exa")

  # ------
  # parser 
  # ------

  @typep dep() :: {
           depth :: E.count(),
           name :: String.t(),
           src :: nil | String.t(),
           req :: nil | String.t()
         }
  @typep index() :: %{String.t() => G.vert()}
  @typep labels() :: %{G.vert() => String.t()}

  @spec parse([dep()], [G.vert()], index(), E.count1(), G.graph(), labels()) ::
          {index(), G.graph(), labels()}

  defp parse([], _stack, index, _n, g, labels), do: {index, g, labels}

  defp parse([{0, name, _req, _src} | deps], [], _, 0, g, _) do
    parse(deps, [1], %{name => 1}, 2, Graf.add(g, 1), %{1 => name})
  end

  defp parse([{d, name, src, req} | deps], stack, index, n, g, labels) do
    {i, new_n, new_index, new_labels} =
      if is_map_key(index, name) do
          # existing node
          {Map.fetch!(index, name), n, index, labels}
      else
          # new node
          label = name <> "\n" <> to_string(src) <> "\n" <> req
          {n, n + 1, Map.put(index, name, n), Map.put(labels, n, label)}
      end

    # reversed DOT arrow?
    g = Graf.add(g, {hd(stack), i})
    next_d = if deps == [], do: d, else: deps |> hd() |> elem(0)

    new_stack =
      cond do
        next_d == d -> stack
        next_d == d + 1 -> [i | stack]
        next_d < d -> Enum.reduce(1..(d - next_d), stack, fn _i, s -> tl(s) end)
      end

    parse(deps, new_stack, new_index, new_n, g, new_labels)
  end

  # -----
  # lexer 
  # -----

  @spec lex(String.t(), E.count()) :: [dep()]

  defp lex("|-- " <> rest, d), do: lex(rest, d + 1)
  defp lex("|   " <> rest, d), do: lex(rest, d + 1)
  defp lex("`-- " <> rest, d), do: lex(rest, d + 1)
  defp lex("    " <> rest, d), do: lex(rest, d + 1)

  defp lex(<<c, _::binary>> = rest, d) when is_ascii(c) do
    {name, rest} = name(rest, "")
    {req, rest} = req(rest, "")
    src = src(rest, "")

    {src, req} =
      cond do
        is_nil(req) and is_nil(src) -> {:root, nil}
        src == "Hex package" -> {:hex, req}
        String.starts_with?(src, "..") -> {:local, src}
        String.starts_with?(src, "http") -> 
          # assumes format:  (url - tag_or_branch)
          # assumes tags are semantically versioned starting with 'v'
          # and branches do not begin with 'v'
          # if no tag or branch is given
          # will default to {:branch, url}
          case src |> String.split() |> List.last() do
            <<?v, _::binary>>=tag -> {:tag, tag}
            ref -> {:branch, ref}
          end
      end

    {d, name, src, req}
  end

  defp lex(str, _d) do
    msg = "Unrecognized DEP data: '#{str}'"
    Logger.error(msg)
    raise ArgumentError, message: msg
  end

  defp name(<<c, s::binary>>, name) when is_namechar(c), do: name(s, <<name::binary, c>>)
  defp name(<<c, ?(, _::binary>> = s, name) when is_ws(c), do: {name, s}
  defp name(<<c, s::binary>>, name) when is_ws(c), do: {name, s}
  defp name(<<>>, name), do: {name, ""}

  defp req(<<>>, ""), do: {nil, ""}
  defp req(<<>>, req), do: {req, ""}
  defp req(<<?\s, ?(, s::binary>>, ""), do: {nil, s}
  defp req(<<?\s, ?(, s::binary>>, req), do: {req, s}
  defp req(<<c, s::binary>>, req), do: req(s, <<req::binary, c>>)

  defp src(<<>>, ""), do: nil
  defp src(")", src), do: src
  defp src(<<c, s::binary>>, src), do: src(s, <<src::binary, c>>)
end
