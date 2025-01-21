defmodule Exa.Graf.GdbTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Std.Mol

  alias Exa.Graf.Graf
  alias Exa.Graf.GrafBuild
  alias Exa.Graf.Gdb

  @out_dir Path.join(["test", "output", "graf", "dot"])

  @in_dir Path.join(["test", "input", "graf", "adj"])

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_adj)

  # render  ---------

  test "simple" do
    gin = Graf.from_adj_file(in_file("fan_in_10"))
    gout = Graf.from_adj_file(in_file("fan_out_10"))
    gwheel = Graf.from_adj_file(in_file("wheel_10"))

    # permute vertices on the fan-in graph
    inperm = Graf.relabel(gin, fn i -> rem(i, 9) + 1 end)

    # build gdb
    gdb = Gdb.new() |> Gdb.add(gin) |> Gdb.add(gout)

    # null test - empty MoL
    qwheel = Gdb.query_isomorphic(gdb, gwheel)
    assert %{} == qwheel

    # equality test
    qeq = Gdb.query_isomorphic(gdb, gin)
    assert %{:isomorphic => [gin]} == qeq

    # permutation isomorphism test
    qin = Gdb.query_isomorphic(gdb, inperm)
    assert %{:isomorphic => [gin]} == qeq
    # Draw.graph(g, @out_dir)
  end

  test "all graphs" do
    # OEIS A000088
    # n:   2 3  4  5   6    7     8      9
    # gdb: 2 4 11 34 156 1044 12346 274668

    do_gdb(3, false, 4)
    do_gdb(4, false, 11)

    # OEIS A001349
    # n:   2 3 4  5   5   7     8      9
    # gdb: 1 2 6 21 112 853 11117 261080

    do_gdb(3, true, 2)
    do_gdb(4, true, 6)
  end

  defp do_gdb(n,connected?, ng) do
    gdb = GrafBuild.all_graphs(n, connected?)
    assert ng == Mol.lengths(gdb)
    # Draw.graph(g, @out_dir)
  end
end
