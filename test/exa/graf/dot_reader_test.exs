defmodule Exa.Graf.DotReaderTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Graf.Agra

  import Exa.Graf.DotReader

  @in_dir ["test", "input", "graf", "dot"]

  @in_files ["abcd", "squares", "petersen"]

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_dot)

  # DOT input ---------

  test "missing file" do
    dne = "File does not exist"
    in_file = in_file("xyz")
    {:error, %File.Error{path: ^in_file, action: ^dne}} = from_dot_file(in_file)
  end

  test "small" do
    {graph, gattrs} = "small" |> in_file() |> from_dot_file()
    assert graph != :error

    edges =
      Enum.sort([
        {1, 2},
        {2, 3},
        {1, 4},
        {1, 5},
        {3, 6},
        {3, 7},
        {4, 6},
        {1, 7},
        {3, 8}
      ])

    assert edges == graph |> Agra.edges() |> Enum.sort()

    assert [alias: "parse"] == gattrs[2]
    assert [alias: "execute"] == gattrs[3]
  end

  test "test123" do
    {graph, gattrs} = "test123" |> in_file() |> from_dot_file()
    assert graph != :error

    # note 7 is missing because it is used as an identifier
    # for the anonymous subgraph that is used for rank similarity
    assert [1, 2, 3, 4, 5, 6, 8, 9] == Agra.verts(graph)
    edges = Enum.sort([{1, 2}, {2, 3}, {1, 4}, {1, 5}, {4, 5}, {2, 4}, {8, 9}])
    assert edges == graph |> Agra.edges() |> Enum.sort()
    assert [alias: "b", shape: "box"] == gattrs[2]

    assert [
             alias: "c",
             style: "filled",
             fontcolor: "red",
             fontname: "Palatino-Italic",
             fontsize: "24",
             color: "blue",
             label: "hello world"
           ] == gattrs[3]

    assert [weight: "100", label: "hi"] == gattrs[{1, 5}]
    assert [label: "multi-line\\nlabel"] == gattrs[{4, 5}]

    assert [fontname: "Helvetica"] == gattrs["test123_node"]
    assert [style: "dashed"] == gattrs["test123_edge"]
    assert [penwidth: "2.0"] == gattrs["test123"]
    assert [rank: "same"] == gattrs["rank_7"]
  end

  test "dot input" do
    for file <- @in_files do
      {_graph, _gattrs} = file |> in_file() |> from_dot_file()
      #     assert graph != :error
      # IO.inspect(graph, label: "graph ")
      # IO.inspect(gattrs, label: "attrs")
    end
  end
end
