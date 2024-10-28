defmodule Exa.Graf.DrawTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Graf.Graf
  alias Exa.Graf.Draw

  # @in_dir ["test", "input", "graf", "dot"]
  # @in_files ["abcd", "squares", "test123", "small", "petersen"]

  @out_dir Path.join(["test", "output", "graf", "dot"])

  # defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_dot)

  # render  ---------

  test "weak components" do
    # 3 components:
    #   1 => 1       isolated
    #   2 => [2,3]   2 -> 3 
    #   4 => [4,5,6] 4 -> 5, 6 -> 5
    g = Graf.build(:adj, "comp123", [1, {2, 3}, {4, 5}, {6, 5}])
    wcomp = Graf.components(g, :weak)
    Draw.by_components(g, wcomp, @out_dir)
  end

  test "strong components" do
    # wikipedia SCC example graph, plus a standalone vertex and a self-loop
      g =        Graf.new(:adj, "wiki_scc")
        |> Graf.add([{1, 2, 3, 1}, {4, 5, 6, 5, 4}, {7, 8, 7}, 9, {10, 10}])
        |> Graf.add([{2, 7}, {3, 7}, {4, 8}, {6, 8}])
    scomp = Graf.components(g, :strong)
    Draw.by_components(g, scomp, @out_dir)
  end
end
