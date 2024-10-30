defmodule Exa.Graf.DrawTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Graf.Graf
  # alias Exa.Graf.GrafBuild
  alias Exa.Graf.Draw

  @out_dir Path.join(["test", "output", "graf", "dot"])

  @in_dir Path.join(["test", "input", "graf", "adj"])

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_adj)

  # render  ---------

  test "big random" do
    # n = 50
    # g = GrafBuild.random(:adj, n, 2 * n) 
    # Graf.to_adj_file(@out_dir)
    g = Graf.from_adj_file(in_file("random_50_100"))
    Draw.graph(g, @out_dir)

    fronts = Graf.frontiers(g, 46)

    attrs =
      Enum.reduce(fronts, %{46 => [{:fontcolor, "black"}]}, fn {ihop, iset}, attrs ->
        Enum.reduce(iset, attrs, fn i, attrs ->
          col = if i == 46, do: "darkred", else: "gray#{min(99, 50 + 5 * ihop)}"
          Exa.Std.Mol.prepends(attrs, i, [{:style, :filled}, {:fillcolor, col}])
        end)
      end)

    g |> Graf.rename("rand_50_100_fronts") |> Draw.partitions(fronts, @out_dir, attrs)

    scomp = Graf.components(g, :strong)
    g |> Graf.rename("rand_50_100_scc") |> Draw.partitions(scomp, @out_dir)
  end

  test "weak components" do
    # 3 components:
    #   1 => 1       isolated
    #   2 => [2,3]   2 -> 3 
    #   4 => [4,5,6] 4 -> 5, 6 -> 5
    g = Graf.build(:adj, "comp123", [1, {2, 3}, {4, 5}, {6, 5}])
    wcomp = Graf.components(g, :weak)
    Draw.partitions(g, wcomp, @out_dir)
  end

  test "strong components" do
    # wikipedia SCC example graph
    g =
      Graf.new(:adj, "wiki_scc")
      |> Graf.add([{1, 2, 3, 1}, {4, 5, 6, 5, 4}, {7, 8, 7}])
      |> Graf.add([{2, 7}, {3, 7}, {4, 8}, {6, 8}])

    scomp = Graf.components(g, :strong)
    Draw.partitions(g, scomp, @out_dir)
  end
end
