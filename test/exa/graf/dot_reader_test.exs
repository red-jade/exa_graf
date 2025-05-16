defmodule Exa.Graf.DotReaderTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Std.Histo2D

  alias Exa.Graf.Adj
  alias Exa.Graf.Gio
  alias Exa.Graf.Graf
  alias Exa.Graf.Morf

  import Exa.Graf.Gio.DotReader

  @in_dir Path.join(["test", "input", "graf", "dot"])
  @out_dir Path.join(["test", "output", "graf", "adj"])

  @in_files ["abcd", "squares", "petersen"]

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_dot)

  # DOT input ---------

  test "missing file" do
    dne = "File does not exist"
    in_file = in_file("xyz")
    {:error, %File.Error{path: ^in_file, action: ^dne}} = Gio.from_dot_file(in_file)
  end

  test "small" do
    {graph, gattrs} = "small" |> in_file() |> Gio.from_dot_file()
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

    assert edges == graph |> Adj.edges() |> Enum.sort()

    assert [alias: "parse"] == gattrs[2]
    assert [alias: "execute"] == gattrs[3]
  end

  test "test123" do
    {graph, gattrs} = "test123" |> in_file() |> from_dot_file()
    assert graph != :error

    # note 7 is missing because it is used as an identifier
    # for the anonymous subgraph that is used for rank similarity
    assert [1, 2, 3, 4, 5, 6, 8, 9] == Adj.verts(graph)
    edges = Enum.sort([{1, 2}, {2, 3}, {1, 4}, {1, 5}, {4, 5}, {2, 4}, {8, 9}])
    assert edges == graph |> Adj.edges() |> Enum.sort()
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
      {graph, _gattrs} = file |> in_file() |> Gio.from_dot_file()
      #     assert graph != :error
      # IO.inspect(graph, label: "graph ")
      # IO.inspect(gattrs, label: "attrs")
      Gio.to_adj_file(graph, @out_dir)
    end
  end

  test "dot input petersen" do
    {peter, _gattrs} = "petersen" |> in_file() |> Gio.from_dot_file()
    h = Morf.degree_histo2d(peter)
    assert %{{3, 3} => 10} == h
    assert {:homo, {3, 3}} == Histo2D.homogeneous(h)

    for fmt <- [:adj, :dig] do
      g = Graf.convert(peter, fmt)
      p1_1 = Graf.reachable(g, 1, :out, 1)
      assert MapSet.new([1, 2, 5, 6]) == p1_1
      p1_2 = Graf.reachable(g, 1, :out, 2)
      assert MapSet.new(1..10) == p1_2
    end
  end
end
