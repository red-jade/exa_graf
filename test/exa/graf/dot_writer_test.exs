defmodule Exa.Graf.DotWriterTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Color.Col3b
  alias Exa.Color.Col3f

  import Exa.Graf.DotWriter

  @out_dir ["test", "output", "graf", "dot"]
  defp file(name), do: Exa.File.join(@out_dir, name, @filetype_dot)

  @simple_dot ~s|digraph simple {
  node [penwidth=1];
  edge [style=solid];
  rankdir=TB;
  1 [label="one", color="red"];
  2;
  3;
  4;
  1 -> 2 [label="edge", color="0.1,0.4,0.8"];
  2 -> 3 [color="#FF0000"];
  1 -> 4;
  4 -> 3;
  4 -> 1;
  1 -> 4 -> 3 -> 1;
}
|

  # DOT output ---------

  test "simple" do
    col3f = Col3f.new(0.1, 0.4, 0.8)
    col3b = Col3b.new(255, 0, 0)

    dot =
      new_dot("simple")
      |> node(:node, penwidth: 1)
      |> node(:edge, style: :solid)
      |> rankdir(:TB)
      |> node(1, label: "one", color: "red")
      |> nodes([2, 3, 4])
      |> edge({1, 2}, label: "edge", color: col3f)
      |> edge({2, 3}, color: col3b)
      |> edges([{1, 4}, {4, 3}, {4, 1}])
      |> chain([1, 4, 3, 1])
      |> end_dot()
      |> to_file(file("simple"))

    assert @simple_dot == to_string(dot)
  end

  test "graph attributes" do
    name = "simple"
    col3f = Col3f.new(0.1, 0.4, 0.8)
    col3b = Col3b.new(255, 0, 0)

    gattrs = %{
      :node => [penwidth: 1],
      :edge => [style: :solid],
      name => [rankdir: :TB],
      1 => [label: "one", color: "red"],
      {1, 2} => [label: "edge", color: col3f],
      {2, 3} => [color: col3b]
    }

    dot =
      new_dot(name)
      |> globals(name, gattrs)
      |> node(1, gattrs)
      |> nodes([2, 3, 4])
      |> edge({1, 2}, gattrs)
      |> edge({2, 3}, gattrs)
      |> edges([{1, 4}, {4, 3}, {4, 1}])
      |> chain([1, 4, 3, 1])
      |> end_dot()
      |> to_file(file("simple"))

    assert @simple_dot == to_string(dot)
  end
end
