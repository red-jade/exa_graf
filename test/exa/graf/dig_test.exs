defmodule Exa.Graf.DigTest do
  use ExUnit.Case

  use Exa.Graf.Constants
  alias Exa.Graf.Dig

  alias Exa.Std.Histo2D

  @in_dot_dir ["test", "input", "graf", "dot"]

  defp dot_file(name), do: Exa.File.join(@in_dot_dir, name, @filetype_dot)

  test "simple" do
    # 2 verts, 1 edge: 8 -> 9
    g = Dig.new("simple")

    Dig.add(g, 8)
    assert Dig.vert?(g, 8)

    Dig.add(g, 9)
    assert Dig.vert?(g, 9)

    Dig.add(g, {8, 9})

    assert 2 == Dig.nvert(g)
    assert 1 == Dig.nedge(g)

    assert true == Dig.vert?(g, 8)
    assert true == Dig.edge?(g, {8, 9})

    assert false == Dig.vert?(g, 88)
    assert false == Dig.edge?(g, {88, 99})

    assert [8, 9] == Dig.verts(g)
    assert [{8, 9}] == Dig.edges(g)

    assert {8, 0} == Dig.degree(g, 8, :in)
    assert {8, 1} == Dig.degree(g, 8, :out)
    assert {8, 0, 1} == Dig.degree(g, 8, :inout)

    assert {9, 1, 0} == Dig.degree(g, 9, :inout)

    assert {8, [], [9]} == Dig.neighborhood(g, 8, :inout)
    assert {9, [8], []} == Dig.neighborhood(g, 9, :inout)

    Dig.delete(g)
  end

  test "repeat vertex" do
    g = Dig.new("repeat vert")
    Dig.add(g, 8)
    assert Dig.vert?(g, 8)

    # no error on repeated vertex
    Dig.add(g, 8)
    assert Dig.vert?(g, 8)

    assert 1 == Dig.nvert(g)

    Dig.delete(g)
  end

  test "edge create vertex" do
    g = Dig.new("edge vert")
    Dig.add(g, {8, 9})
    assert 2 == Dig.nvert(g)
    assert 1 == Dig.nedge(g)
    assert [{8, 9}] == Dig.edges(g)

    Dig.delete(g)
  end

  test "repeat edge" do
    g = Dig.new("repeat edge")
    Dig.add(g, {8, 9})
    Dig.delete(g)
  end

  test "self loop and cycles" do
    g = Dig.new("selfc", [], :cyclic)
    ^g = Dig.add(g, {8, 8})
    Dig.delete(g)

    g = Dig.new("selfa", [], :acyclic)
    assert {:error, "Cyclic path [8, 8]"} == Dig.add(g, {8, 8})
    ^g = Dig.add(g, {4, 5})
    assert {:error, "Cyclic path [4, 5]"} == Dig.add(g, {5, 4})
    Dig.delete(g)
  end

  test "delete edge" do
    g = Dig.new("del edge", [{8, 9}])
    assert Dig.edge?(g, {8, 9})
    assert 2 == Dig.nvert(g)
    assert 1 == Dig.nedge(g)

    Dig.delete(g, {8, 9})
    assert 2 == Dig.nvert(g)
    assert 0 == Dig.nedge(g)
    assert not Dig.edge?(g, {8, 9})

    # not an error to remove it again
    assert g == Dig.delete(g, {8, 9})
    Dig.delete(g)
  end

  test "delete vertex" do
    g = Dig.new("del vert", [{8, 9}])
    assert 2 == Dig.nvert(g)
    assert 1 == Dig.nedge(g)
    assert Dig.vert?(g, 8)
    assert Dig.vert?(g, 9)
    assert Dig.edge?(g, {8, 9})

    Dig.delete(g, 9)
    assert 1 == Dig.nvert(g)
    assert 0 == Dig.nedge(g)
    assert Dig.vert?(g, 8)
    assert not Dig.vert?(g, 9)
    assert not Dig.edge?(g, {8, 9})

    # not an error
    assert g == Dig.delete(g, 13)
    Dig.delete(g)
  end

  test "hash isomorphism" do
    # build different graphs on 3 nodes

    # 1-2 fan out and 2-1 fans in

    g2a = Dig.new("hash iso 1")
    Dig.add(g2a, [{1, 2}, {1, 3}])
    assert 3 = Dig.nvert(g2a)
    assert 2 = Dig.nedge(g2a)
    h2a = Dig.hash(g2a)
    assert :undecided == Dig.isomorphic?(g2a, g2a)

    g2b = Dig.new("hash iso 2")
    Dig.add(g2b, [{2, 1}, {3, 1}])
    assert 3 = Dig.nvert(g2b)
    assert 2 = Dig.nedge(g2b)
    h2b = Dig.hash(g2b)
    assert :undecided == Dig.isomorphic?(g2b, g2b)

    assert h2a != h2b
    assert false == Dig.isomorphic?(g2a, g2b)

    Dig.delete(g2a)
    Dig.delete(g2b)

    # 3-cycles

    g2a = Dig.new("hash iso 3")
    Dig.add(g2a, [{1, 2}, {2, 3}, {3, 1}])
    assert 3 = Dig.nvert(g2a)
    assert 3 = Dig.nedge(g2a)
    h2a = Dig.hash(g2a)
    assert :undecided == Dig.isomorphic?(g2a, g2a)

    g2b = Dig.new("hash iso 4")
    Dig.add(g2b, [{4, 5}, {5, 6}, {6, 4}])
    assert 3 = Dig.nvert(g2b)
    assert 3 = Dig.nedge(g2b)
    h2b = Dig.hash(g2b)
    assert :undecided == Dig.isomorphic?(g2b, g2b)

    assert h2a == h2b
    assert :undecided == Dig.isomorphic?(g2a, g2b)

    Dig.delete(g2a)
    Dig.delete(g2b)
  end

  test "read DOT petersen" do
    {g, _gattrs} = "petersen" |> dot_file() |> Dig.from_dot_file()

    assert 10 == Dig.nvert(g)
    assert 30 == Dig.nedge(g)
    h2inout = g |> Dig.degree_histo2d() |> Histo2D.to_list()
    assert h2inout == [{{3, 3}, 10}]
    Dig.delete(g)
  end

  @small_edges Enum.sort([
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

  @small_histo [
    {{0, 4}, 1},
    {{1, 0}, 2},
    {{1, 1}, 2},
    {{1, 3}, 1},
    {{2, 0}, 2}
  ]

  test "read DOT small" do
    {g, _gattrs} = "small" |> dot_file() |> Dig.from_dot_file()

    assert 8 == Dig.nvert(g)
    assert 9 == Dig.nedge(g)
    assert @small_edges = g |> Dig.edges() |> Enum.sort()
    h2inout = g |> Dig.degree_histo2d() |> Histo2D.to_list()
    assert @small_histo == h2inout
    Dig.delete(g)
  end
end
