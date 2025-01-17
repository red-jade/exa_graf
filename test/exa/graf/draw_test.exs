defmodule Exa.Graf.DrawTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Std.Mol

  alias Exa.Graf.Graf
  # alias Exa.Graf.GrafBuild
  alias Exa.Graf.Draw

  @out_dir Path.join(["test", "output", "graf", "dot"])

  @in_dir Path.join(["test", "input", "graf", "adj"])

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_adj)

  # render  ---------

  test "big random" do
    # g = GrafBuild.random(:adj, 50, 100) |> Graf.rename("rand_50_100")
    # Graf.to_adj_file(g, @in_dir)
    g = Graf.from_adj_file(in_file("rand_50_100"))
    Draw.graph(g, @out_dir)

    fronts = Graf.frontiers(g, 46)

    fattrs =
      Enum.reduce(fronts, %{46 => [{:fontcolor, "black"}]}, fn {ihop, iset}, attrs ->
        Enum.reduce(iset, attrs, fn i, attrs ->
          col = if i == 46, do: "darkred", else: "gray#{min(99, 50 + 5 * ihop)}"
          Mol.prepends(attrs, i, style: :filled, fillcolor: col)
        end)
      end)

    gfront = Graf.rename(g, "rand_50_100_fronts")
    Draw.partitions(gfront, fronts, @out_dir, fattrs, false)
    gflayer = Graf.rename(g, "rand_50_100_flayer")
    Draw.partitions(gflayer, fronts, @out_dir, fattrs, true)

    scomp = Graf.components(g, :strong)
    gscc = Graf.rename(g, "rand_50_100_scc")
    Draw.partitions(gscc, scomp, @out_dir)
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

    dff = Graf.spanning_forest(g, :strong)
    trees = Graf.forest_graph(dff, "wiki_trees")
    Draw.graph(trees, @out_dir)

    all_edges = g |> Graf.edges() |> MapSet.new()
    tre_edges = trees |> Graf.edges() |> MapSet.new()
    cobwebs = MapSet.difference(all_edges, tre_edges)

    eattrs =
      Enum.reduce(cobwebs, Mol.new(), fn e, attrs ->
        Mol.prepends(attrs, e, color: "gray80")
      end)

    part = Graf.forest_partition(dff)
    Draw.partitions(g |> Graf.rename("wiki_forest"), part, @out_dir, eattrs)
  end
end
