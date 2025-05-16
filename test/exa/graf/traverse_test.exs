defmodule Exa.Graf.TraverseTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Graf.Dff
  alias Exa.Graf.Graf
  alias Exa.Graf.Traverse

  # TODO - need more complex test cases here
  #        forest trees should be fan-out, not just linear chains

  # wikipedia directed graph example
  # many directed 3- and 2-cycles
  @wiki Graf.new(:adj, "wiki")
        |> Graf.add([{1, 2, 3, 1}, {4, 5, 6, 5, 4}, {7, 8, 7}])
        |> Graf.add([{2, 7}, {3, 7}, {4, 8}, {6, 8}])

  test "simple" do
    # simple rooted dag
    dag = Graf.build(:adj, "dag", [{1, 2}, {1, 3}, {3, 4}, {3, 5}, {4, 5}])
    dff = Dff.new(dag, :strong, 1)
    assert %{1 => [2, 3], 3 => [4], 4 => [5], :forest => [1]} = dff

    # Graf.dump_forest(dff)
    pre = Dff.preorder(dff)
    assert [1, 2, 3, 4, 5] == pre
    post = Dff.postorder(dff)
    assert [2, 5, 4, 3, 1] == post
  end

  test "wikipedia scc" do
    weak_dff = Dff.new(@wiki, :weak)

    expect = %{
      1 => [2],
      2 => [3],
      3 => [7],
      4 => [5],
      5 => [6],
      7 => [8],
      8 => [4],
      :forest => [1]
    }

    assert expect == weak_dff

    # Graf.dump_forest(weak_dff)
    pre = Dff.preorder(weak_dff)
    assert [1, 2, 3, 7, 8, 4, 5, 6] == pre
    post = Dff.postorder(weak_dff)
    assert [6, 5, 4, 8, 7, 3, 2, 1] == post

    strong_dff = Dff.new(@wiki, :strong)

    assert %{1 => [2], 2 => [3], 3 => [7], 4 => [5], 5 => [6], 7 => [8], :forest => [1, 4]} ==
             strong_dff

    # Graf.dump_forest(strong_dff)

    pre = Dff.preorder(strong_dff)
    assert [1, 2, 3, 7, 8, 4, 5, 6] == pre

    post = Dff.postorder(strong_dff)
    assert [8, 7, 3, 2, 1, 6, 5, 4] == post

    trees = Dff.new(strong_dff, :strong)
    assert [{1, 2}, {2, 3}, {3, 7}, {4, 5}, {5, 6}, {7, 8}] = Enum.sort(Graf.edges(trees))

    part = Dff.to_partition(strong_dff)
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

    assert not Graf.tree?(@wiki, :weak)
    assert false == Graf.tree?(@wiki, :strong)
    assert Graf.cyclic?(@wiki, :weak)
    assert Graf.cyclic?(@wiki, :strong)
  end

  test "dfs bfs" do
    # preorder

    pre = %Pre{}

    # single path in one weakly connected component
    dfs = Traverse.graph(@wiki, :weak, :dfs, pre, 1)
    assert [1, 2, 3, 7, 8, 4, 5, 6] == dfs

    # 2 strongly connected components
    # so the restart at 4 is arbitrary
    dfs = Traverse.graph(@wiki, :strong, :dfs, pre, 1)
    assert [1, 2, 3, 7, 8, 4, 5, 6] == dfs

    # single traversal in one weakly connected component
    bfs = Traverse.graph(@wiki, :weak, :bfs, pre, 4)
    assert [4, 5, 8, 6, 7, 2, 3, 1] == bfs

    # 2 strongly connected components
    # so the restart at 1 is arbitrary
    bfs = Traverse.graph(@wiki, :strong, :bfs, pre, 4)
    assert [4, 5, 8, 6, 7, 1, 2, 3] == bfs
  end

  # test "sssp" do
  #   sssp18 = Graf.sssp(@wiki, 1, 8, :strong)
  #   assert [1, 2, 7, 8] == sssp18

  #   sssp58 = Graf.sssp(@wiki, 5, 8, :strong)
  #   IO.inspect(sssp58)
  #   assert sssp58 in [[5, 4, 8], [5, 6, 8]]

  #   # TODO - should fail
  #   sssp14 = Graf.sssp(@wiki, 1, 4, :strong)
  #   assert [] == sssp18
  # end

  # test "dijkstra" do
  #   sssp18 = Graf.dijkstra(@wiki, 1, 8, :strong)
  #   assert [1, 2, 7, 8] == sssp18

  #   sssp58 = Graf.dijkstra(@wiki, 5, 8, :strong)
  #   IO.inspect(sssp58)
  #   assert sssp58 in [[5, 4, 8], [5, 6, 8]]

  #   # TODO - should fail
  #   sssp14 = Graf.dijkstra(@wiki, 1, 4, :strong)
  #   assert [] == sssp18
  # end
end
