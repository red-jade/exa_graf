defmodule Exa.Graf.DrawTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Graf.Graf
  alias Exa.Graf.Draw

  @out_dir Path.join(["test", "output", "graf", "dot"])

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
    # wikipedia SCC example graph
    g =
      Graf.new(:adj, "wiki_scc")
      |> Graf.add([{1, 2, 3, 1}, {4, 5, 6, 5, 4}, {7, 8, 7}])
      |> Graf.add([{2, 7}, {3, 7}, {4, 8}, {6, 8}])

    scomp = Graf.components(g, :strong)
    Draw.by_components(g, scomp, @out_dir)
  end
end
