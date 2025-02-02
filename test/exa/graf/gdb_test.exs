defmodule Exa.Graf.GdbTest do
  use ExUnit.Case

  use Exa.Graf.Constants

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
    assert %{:isomorphic => [qin]} == qeq
    # Draw.graph(g, @out_dir)
  end

  # n=5 : loop of 3 + tail of 2
  @g32 Graf.build(:adj, "5_3_2", [
         {1, 2},
         {2, 1},
         {2, 3},
         {3, 2},
         {3, 1},
         {1, 3},
         {3, 4},
         {4, 3},
         {4, 5},
         {5, 4}
       ])

  # n=5 : loop of 4 + tail of 1
  @g41 Graf.build(:adj, "5_4_1", [
         {1, 2},
         {2, 1},
         {2, 3},
         {3, 2},
         {3, 4},
         {4, 3},
         {4, 1},
         {1, 4},
         {4, 5},
         {5, 4}
       ])

  test "homeomorphic" do
    g32 = @g32
    g41 = @g41

    g32perm =
      g32
      |> Graf.relabel(fn i -> rem(i, 5) + 1 end)
      |> Graf.rename("5_3_2_perm")

    # make a GDB from:
    # - isomorphic pair g32 and g32 permuted
    # - homeomorphic g41
    {:gdb, isos, homeos, contras} =
      gdb =
      Gdb.new()
      |> Gdb.add(g32)
      |> Gdb.add(g32perm)
      |> Gdb.add(g41)

    k32 = Graf.gkey(g32)
    k41 = Graf.gkey(g41)

    g32con = Graf.contract_linears(g32)
    g32permcon = Graf.contract_linears(g32perm)
    g41con = Graf.contract_linears(g41)

    kcon = Graf.gkey(g32con)

    assert %{
             ^k41 => [^g41],
             ^k32 => [^g32, ^g32perm]
           } = isos

    assert %{^kcon => [^g32con, ^g32permcon, ^g41con]} = homeos

    assert %{
             ^g32con => [^g32],
             ^g32permcon => [^g32perm],
             ^g41con => [^g41]
           } = contras

    # expect g32 and g32 permuted
    g32isos = Gdb.query_isomorphic(gdb, @g32)
    assert %{:isomorphic => [^g32, ^g32perm]} = g32isos

    # expect all 3 graphs g32, g32 permuted and g41
    g32homeos = Gdb.query_homeomorphic(gdb, @g32)
    assert %{:homeomorphic => [^g32, ^g32perm, ^g41]} = g32homeos

  end

  test "all graphs" do
    # OEIS A000088
    # n:   2 3  4  5   6    7     8      9
    # gdb: 2 4 11 34 156 1044 12346 274668

    # do_gdb(3, false, 4, 4)  |>  IO.inspect()
    # do_gdb(4, false, 11, 11)
    do_gdb(5, false, 34, 32)

    # OEIS A001349
    # n:   2 3 4  5   5   7     8      9
    # gdb: 1 2 6 21 112 853 11117 261080

    # do_gdb(3, true, 2, 2) |>     IO.inspect()
    # do_gdb(4, true, 6, 6)
    _gdb5 = do_gdb(5, true, 21, 19)

    # Gdb.query_isomorphic(gdb5, @g32) |> IO.inspect(label: "isomorf g32")

    # Gdb.query_homeomorphic(gdb5, @g32) |> IO.inspect(label: "homeomorf g32")

    # Gdb.query_isomorphic(gdb5, @g41) |> IO.inspect(label: "isomorf g41")

    # Gdb.query_homeomorphic(gdb5, @g41) |> IO.inspect(label: "homeomorf g41")
  end

  defp do_gdb(n, connected?, ng, nh) do
    {:gdb, _, _, contras} = gdb = GrafBuild.all_graphs(n, connected?)
    IO.inspect(Gdb.ngraph(gdb), label: "n #{n} graphs")
    IO.inspect(Gdb.niso(gdb),   label: "n #{n} isos")
    IO.inspect(Gdb.nhomeo(gdb), label: "n #{n} homeos")

    assert ng == Gdb.niso(gdb)
    assert nh == Gdb.nhomeo(gdb)
    # Draw.graph(g, @out_dir)
    Enum.each(contras, fn {c, gs} ->
      names = Enum.map(gs, &Graf.name/1)
      if Enum.count(gs) > 1 do
        IO.inspect("#{Graf.name(c)} => #{names}")
      end
    end)
    gdb
  end
end
