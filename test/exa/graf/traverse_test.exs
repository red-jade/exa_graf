defmodule Exa.Graf.TraverseTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Graf.Traverse
  alias Exa.Graf.Traverse.Visitor

  alias Exa.Graf.Graf

  # TODO - need more complex test cases here
  #        forest trees should be fan-out, not just linear chains

  test "simple" do
    # simple rooted dag
    dag = Graf.build(:adj, "dag", [{1, 2}, {1, 3}, {3, 4}, {3, 5}, {4, 5}])
    dff = Graf.spanning_forest(dag, :strong, 1)
    assert %{1 => [2, 3], 3 => [4], 4 => [5], :forest => [1]} = dff

    # Graf.dump_forest(dff)
    pre = Graf.preorder(dff)
    assert [1, 2, 3, 4, 5] == pre
    post = Graf.postorder(dff)
    assert [2, 5, 4, 3, 1] == post
  end

  test "wikipedia scc" do
    wik =
      Graf.new(:adj, "wik")
      |> Graf.add([{1, 2, 3, 1}, {4, 5, 6, 5, 4}, {7, 8, 7}])
      |> Graf.add([{2, 7}, {3, 7}, {4, 8}, {6, 8}])

    weak_dff = Graf.spanning_forest(wik, :weak)

    assert %{1 => [2], 2 => [3], 3 => [7], 4 => [5], 5 => [6], 7 => [8], 8 => [4], :forest => [1]} ==
             weak_dff

    # Graf.dump_forest(weak_dff)
    pre = Graf.preorder(weak_dff)
    assert [1, 2, 3, 7, 8, 4, 5, 6] == pre
    post = Graf.postorder(weak_dff)
    assert [6, 5, 4, 8, 7, 3, 2, 1] == post

    strong_dff = Graf.spanning_forest(wik, :strong)

    assert %{1 => [2], 2 => [3], 3 => [7], 4 => [5], 5 => [6], 7 => [8], :forest => [1, 4]} ==
             strong_dff

    # Graf.dump_forest(strong_dff)

    pre = Graf.preorder(strong_dff)
    assert [1, 2, 3, 7, 8, 4, 5, 6] == pre

    post = Graf.postorder(strong_dff)
    assert [8, 7, 3, 2, 1, 6, 5, 4] == post

    trees = Graf.forest_graph(strong_dff)
    assert [{1, 2}, {2, 3}, {3, 7}, {4, 5}, {5, 6}, {7, 8}] = Enum.sort(Graf.edges(trees))

    part = Graf.forest_partition(strong_dff)
    assert %{1 => MapSet.new([1, 2, 3, 7, 8]), 4 => MapSet.new([4, 5, 6])} == part
  end

  test "cyclic" do
    # simple directed tree
    tree = Graf.build(:adj, "dag", [{1, 2}, {1, 3}, {3, 4}, {3, 5}])
    assert Graf.tree?(tree, :weak)
    assert 1 == Graf.root(tree)
    assert not Graf.cyclic?(tree, :weak)
    assert not Graf.cyclic?(tree, :strong)

    # simple tree with last edge flipped
    rev = Graf.build(:adj, "dag", [{1, 2}, {1, 3}, {3, 4}, {5, 3}])
    assert Graf.tree?(rev, :weak)
    assert false == Graf.tree?(rev, :strong)
    assert not Graf.cyclic?(rev, :weak)
    assert not Graf.cyclic?(rev, :strong)

    # simple rooted dag, tree with extra cross edge 
    dag = Graf.build(:adj, "dag", [{1, 2}, {1, 3}, {3, 4}, {3, 5}, {4, 5}])
    assert not Graf.tree?(dag, :weak)
    assert false == Graf.tree?(rev, :strong)
    assert Graf.cyclic?(dag, :weak)
    assert not Graf.cyclic?(dag, :strong)

    # many directed 3- and 2-cycles
    wik =
      Graf.new(:adj, "wik")
      |> Graf.add([{1, 2, 3, 1}, {4, 5, 6, 5, 4}, {7, 8, 7}])
      |> Graf.add([{2, 7}, {3, 7}, {4, 8}, {6, 8}])

    assert not Graf.tree?(wik, :weak)
    assert false == Graf.tree?(wik, :strong)
    assert Graf.cyclic?(wik, :weak)
    assert Graf.cyclic?(wik, :strong)
  end

  test "dfs" do
    # many directed 3- and 2-cycles
    wik =
      Graf.new(:adj, "wik")
      |> Graf.add([{1, 2, 3, 1}, {4, 5, 6, 5, 4}, {7, 8, 7}])
      |> Graf.add([{2, 7}, {3, 7}, {4, 8}, {6, 8}])

    cb = %Visitor{
      init_state: fn _ -> [] end,
      pre_node: fn is, _g, i -> [i | is] end,
      visit_node: fn is, _g, i -> [i | is] end,
      final_result: fn is -> Enum.reverse(is) end
    }

    dfs = Traverse.graph(wik, :dfs, :weak, cb, 1)
    assert [1, 2, 3, 7, 8, 4, 5, 6] == dfs

    bfs = Traverse.graph(wik, :bfs, :weak, cb, 1)
    assert [1, 2, 3, 7, 8, 4, 6, 5] == bfs
  end
end
