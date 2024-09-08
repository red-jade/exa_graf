defmodule Exa.Graf.GrafTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  import Exa.Graf.Graf

  alias Exa.Std.Mol

  @in_dir Path.join(["test", "input", "graf", "adj"])

  defp adj_file(name), do: Exa.File.join(@in_dir, name, @filetype_adj)

  # add ----------

  test "add vert" do
    builder(
      fn tag -> new(tag, "vert") |> add(1) |> add(2) |> add(3) end,
      {:adj, "vert",
       {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
        %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "add verts" do
    builder(
      fn tag -> new(tag, "verts") |> add([1, 2, 3]) end,
      {:adj, "verts",
       {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
        %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "add range" do
    builder(
      fn tag -> new(tag, "range") |> add(1..3) end,
      {:adj, "range",
       {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
        %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "add edge" do
    builder(
      fn tag -> new(tag, "edge") |> add({1, 2}) |> add({2, 3}) |> add({1, 3}) end,
      {:adj, "edge",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "add edges" do
    builder(
      fn tag -> new(tag, "edges") |> add([{1, 2}, {2, 3}, {1, 3}]) end,
      {:adj, "edges",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "add adj" do
    builder(
      fn tag -> new(tag, "adj") |> add({1, [2, 3]}) |> add({2, [3]}) end,
      {:adj, "adj",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "add self loop" do
    graphs =
      builder(
        fn tag -> new(tag, "self loop") |> add(1) |> add({1, 1}) end,
        {:adj, "self_loop", {%{1 => MapSet.new([1])}, %{1 => MapSet.new([1])}}}
      )

    for g <- graphs, do: assert(classify(g, 1) == :self_isolated)
  end

  test "add repeated vert" do
    builder(
      fn tag -> new(tag, "vert") |> add(1) |> add(1) |> add([3, 3]) end,
      {:adj, "vert",
       {%{1 => MapSet.new([]), 3 => MapSet.new([])}, %{1 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "add repeated edge" do
    builder(
      fn tag -> new(tag, "edge") |> add({1, 2}) |> add({1, 2}) |> add([{1, 3}, {1, 3}]) end,
      {:adj, "edge",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  # remove ----------

  defp tri(tag), do: build(tag, "tri", [{1, 2}, {2, 3}, {1, 3}])

  test "valid tri" do
    builder(
      &tri/1,
      {:adj, "tri",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "del vert" do
    builder(
      fn tag -> tri(tag) |> delete(1) end,
      {:adj, "tri",
       {%{2 => MapSet.new([]), 3 => MapSet.new([2])},
        %{2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "del edge" do
    builder(
      fn tag -> tri(tag) |> delete({1, 3}) end,
      {:adj, "tri",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([2])},
        %{1 => MapSet.new([2]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "del adj" do
    builder(
      fn tag -> tri(tag) |> delete({1, [2, 3]}) end,
      {:adj, "tri",
       {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([2])},
        %{1 => MapSet.new([]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "del list" do
    builder(
      fn tag -> tri(tag) |> delete([1, {2, 3}]) end,
      {:adj, "tri",
       {%{2 => MapSet.new([]), 3 => MapSet.new([])}, %{2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "collapse edge" do
    for tag <- [:adj, :dig] do
      g = new(tag, "collapse") |> add([{1, 2}, {2, 2}, {3, 2}, {2, 4}])
      g = contract_edge(g, {1, 2})
      assert [1, 3, 4] == g |> verts() |> Enum.sort()
      assert [{1, 1}, {1, 4}, {3, 1}] == g |> edges() |> Enum.sort()
    end
  end

  # classification ----------

  test "classify" do
    for tag <- [:adj, :dig] do
      g =
        new(tag, "class")
        |> add(1..7)
        # linear chain
        |> add([{1, 2}, {2, 3}, {3, 4}, {4, 5}])
        # two extra nodes on 2
        |> add([{6, 2}, {2, 7}])
        # self loops
        |> add([{4, 4}, {6, 6}, {7, 7}])

      assert [:source, :complex, :linear, :self_linear, :sink, :self_source, :self_sink] ==
               Enum.map(1..7, &classify(g, &1))
    end
  end

  # components, reachable ----------

  test "tree" do
    Enum.each([:adj, :dig], fn tag ->
      # the 5,3 edge is reversed, so not a strong tree
      g = build(tag, "tree", [{1, 2}, {1, 3}, {3, 4}, {5, 3}])

      assert MapSet.new([1, 2, 3, 4]) == reachable(g, 1)
      assert MapSet.new([3, 4, 5]) == reachable(g, 5)
      assert MapSet.new([1, 3, 4, 5]) == reachable(g, 4, :in)
      assert MapSet.new([1, 3, 4, 5]) == reachable(g, 3, :in_out)
      assert connected?(g)
      assert tree?(g)

      g = delete(g, {1, 2})
      assert not tree?(g)
    end)
  end

  test "components" do
    Enum.each([:adj, :dig], fn tag ->
      # 3 components:
      #   1       isolated
      #   [2,3]   2 -> 3 
      #   [4,5,6] 4 -> 5, 6 -> 5
      g = build(tag, "comp123", [1, {2, 3}, {4, 5}, {6, 5}])

      assert :isolated == classify(g, 1)
      assert :source == classify(g, 2)
      assert :sink == classify(g, 3)

      assert not connected?(g)
      assert MapSet.new([2, 3]) == reachable(g, 2)
      assert MapSet.new([4, 5]) == reachable(g, 4)
      assert MapSet.new([2, 3]) == reachable(g, 3, :in)
      assert MapSet.new([4, 5, 6]) == reachable(g, 5, :in)
      comps = components(g)
      assert 3 = map_size(comps)
      assert %{1 => [1], 2 => [2, 3], 4 => [4, 5, 6]} == Mol.sort(comps)
    end)
  end

  test "reverse" do
    for tag <- [:adj, :dig] do
      g = new(tag, "cyc3") |> add([{1, 2}, {2, 3}, {3, 1}, {1, 1}])
      rev = reverse(g)
      assert "cyc3_rev" == name(rev) 
      assert [1,2,3] == rev |> verts() |> Enum.sort()
      assert [{1,1}, {1,3}, {2,1}, {3,2}] == rev |> edges() |> Enum.sort()
    end
  end

  test "iso" do
    for nhop <- [0, 1] do
      # build 3-cycle, with self-loops on different vertices
      g1 = new(:adj, "iso1") |> add([{1, 2}, {2, 3}, {3, 1}, {1, 1}])
      h1 = hash(g1, nhop)

      g2 = new(:adj, "iso2") |> add([{1, 2}, {2, 3}, {3, 1}, {2, 2}])
      h2 = hash(g2, nhop)

      g3 = new(:adj, "iso3") |> add([{1, 2}, {2, 3}, {3, 1}, {3, 3}])
      h3 = hash(g3, nhop)

      assert h1 == h2
      assert h2 == h3
      assert h3 == h1
      assert :undecided = isomorphic?(g1, g2)
      assert :undecided = isomorphic?(g2, g3)
      assert :undecided = isomorphic?(g3, g1)
    end

    # try two versions of the Petersen graph
    # with a self-loop on different verts
    peter = "petersen" |> adj_file() |> from_adj_file()
    pA = add(peter, {1, 1})
    pB = add(peter, {5, 5})
    assert :undecided == isomorphic?(pA, pB)

    retep = reverse(peter)
    assert :undecided == isomorphic?(peter, retep)

    # first example where the 0,1 hop hashes are different
    # 4-cycle with 2 handles, one in reversed direction
    cychan = new(:adj, "cychan") |> add([{1,2},{2,3},{3,4},{4,1},{4,5}, {5,1}])
    cychanA = add(cychan, [{3,6}, {6,2}])
    cychanB = add(cychan, [{6,3}, {2,6}])

    assert hash(cychanA, 0) == hash(cychanB, 0)
    assert hash(cychanA, 1) != hash(cychanB, 1)
    assert not isomorphic?(cychanA, cychanB)
  end

  test "homeomorphism" do
    # loop and tail with different linear chains
    lt1 = new(:adj, "lt1") |> add([{1,2},{2,3},{3,1}, {3,4},{4,5}, {5,6}])
    lt2 = new(:adj, "lt2") |> add([{1,2},{2,3},{3,4},{4,1}, {4,5}, {5,6}])

    assert degree_histo3d(lt1) == degree_histo3d(lt1) 
    assert :undecided == homeomorphic?(lt1, lt2)

    # two loops linked
    ll  = new(:adj, "ll") |> add([{1,2},{2,3},{3,1}, {3,4}, {4,5}, {5,6}, {6,4}])
    assert not homeomorphic?(lt1, ll)
  end

  # join ----------

  test "join" do
    for tag <- [:adj, :dig] do
      # 3 components
      v1 = [1, 2, 3, 4, 5, 6]
      e1 = [{2, 3}, {4, 5}, {6, 5}]
      g1 = build(tag, "orig", [1, e1])
      # line joining 3 components
      # and one edge the same as g1
      e2 = [{1, 2}, {2, 4}, {4, 5}]
      g2 = build(tag, "add", e2)

      g3 = join(g1, g2, :merge)
      assert MapSet.new(verts(g1) ++ verts(g2)) == MapSet.new(verts(g3))
      assert MapSet.new(edges(g1) ++ edges(g2)) == MapSet.new(edges(g3))

      # dig will change g1, so we have to recreate it from scratch
      g1 = build(tag, "orig", [1, e1])

      g4 = join(g1, g2, :disjoint)
      # 1,2,4,5 from g2 get lifted to 7,8,10,11
      assert Enum.sort(v1 ++ [7, 8, 10, 11]) == Enum.sort(verts(g4))
      # edgesfrom g2 get lifted to {7,8},{8,10},{10,11}
      assert Enum.sort(e1 ++ [{7, 8}, {8, 10}, {10, 11}]) == Enum.sort(edges(g4))
    end
  end

  # -----------------
  # private functions
  # -----------------

  defp builder(build, result) do
    for tag <- [:adj, :dig] do
      g = build.(tag)
      assert convert(g, :adj) == result
      g
    end
  end
end
