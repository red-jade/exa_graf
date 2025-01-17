defmodule Exa.Graf.GdbTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  # alias Exa.Std.Mol

  alias Exa.Graf.Graf
  # alias Exa.Graf.GrafBuild
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
    IO.inspect(gdb)

    # null test - empty MoL
    qwheel = Gdb.query_isomorphic(gdb, gwheel)
    assert %{} == qwheel

    # equality test
    qeq = Gdb.query_isomorphic(gdb, gin)
    IO.inspect(qeq)
    assert %{:isomorphic => [gin]} == qeq

    # permutation isomorphism test
    qin = Gdb.query_isomorphic(gdb, inperm)
    IO.inspect(qin)
    assert %{:isomorphic => [gin]} == qeq
    # Draw.graph(g, @out_dir)
  end
end
