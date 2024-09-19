defmodule Exa.Graf.DfsTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  import Exa.Graf.Dfs

  alias Exa.Graf.Graf

  # TODO - need more complex test cases here
  #        forest trees should be fan-out, not just linear chains

  test "simple" do
    # simple rooted dag
    dag = Graf.build(:adj, "dag", [{1, 2}, {1, 3}, {3, 4}, {3, 5}, {4, 5}])
    dff = spanning_forest(dag, :out, 1)
    assert %{1 => [2, 3], 3 => [4], 4 => [5], :forest => [1]} = dff

    dump(dff)
    pre = preorder(dff)
    assert [1, 2, 3, 4, 5] == pre
    post = postorder(dff)
    assert [2, 5, 4, 3, 1] == post
  end

  test "wikipedia scc" do
    wik =
      Graf.new(:adj, "wik")
      |> Graf.add([{1, 2, 3, 1}, {4, 5, 6, 5, 4}, {7, 8, 7}])
      |> Graf.add([{2, 7}, {3, 7}, {4, 8}, {6, 8}])

    dff = spanning_forest(wik)
    assert %{1 => [2], 2 => [3], 3 => [7], 4 => [5], 5 => [6], 7 => [8], :forest => [1, 4]} == dff

    dump(dff)
    pre = preorder(dff)
    assert [1, 2, 3, 7, 8, 4, 5, 6] == pre
    post = postorder(dff)
    assert [8, 7, 3, 2, 1, 6, 5, 4] == post
  end
end
