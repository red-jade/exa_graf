defmodule Exa.Graf.GdbTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Std.Histo1D

  alias Exa.Graf.Contract
  alias Exa.Graf.Graf
  alias Exa.Graf.Build
  alias Exa.Graf.Gdb
  alias Exa.Graf.Gio
  alias Exa.Graf.Morf
  alias Exa.Graf.Gio.Draw

  @out_dir Path.join(["test", "output", "graf", "all"])

  @in_dir Path.join(["test", "input", "graf", "adj"])

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_adj)

  # render  ---------

  test "simple" do
    gin = Gio.from_adj_file(in_file("fan_in_10"))
    gout = Gio.from_adj_file(in_file("fan_out_10"))
    gwheel = Gio.from_adj_file(in_file("wheel_10"))

    # permute vertices on the fan-in graph
    inperm = Graf.rotate(gin)

    # build gdb
    gdb = Gdb.new() |> Gdb.add(gin) |> Gdb.add(gout)

    # null test - empty MoL
    qwheel = Gdb.query_isomorphic(gdb, gwheel)
    assert MapSet.new([]) == qwheel

    # equality test
    qeq = Gdb.query_isomorphic(gdb, gin)
    assert MapSet.new([gin]) == qeq

    # permutation isomorphism test
    qin = Gdb.query_isomorphic(gdb, inperm)
    assert MapSet.new([gin]) == qin
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

    g32perm = g32 |> Graf.rotate() |> Graf.rename("5_3_2_perm")

    # make a GDB from:
    # - isomorphic pair g32 and g32 permuted
    # - homeomorphic g41
    {:gdb, isos, homeos, contras} =
      gdb = Gdb.new() |> Gdb.add(g32) |> Gdb.add(g32perm) |> Gdb.add(g41)

    k32 = Morf.gkey(g32)
    k41 = Morf.gkey(g41)

    g32con = Contract.linears(g32)
    g32permcon = Contract.linears(g32perm)
    g41con = Contract.linears(g41)

    kcon = Morf.gkey(g32con)

    assert map_size(isos) == 2
    assert %{g41 => MapSet.new([g41])} == isos[k41]
    assert %{g32 => MapSet.new([g32perm, g32])} == isos[k32]

    assert map_size(homeos) == 1
    assert %{g32con => MapSet.new([g32con, g32permcon, g41con])} == homeos[kcon]

    assert %{
             g32con => MapSet.new([g32]),
             g32permcon => MapSet.new([g32perm]),
             g41con => MapSet.new([g41])
           } == contras

    # expect g32 and g32 permuted
    g32isos = Gdb.query_isomorphic(gdb, @g32)
    assert MapSet.new([g32, g32perm]) == g32isos

    # expect all 3 graphs g32, g32 permuted and g41
    g32homeos = Gdb.query_homeomorphic(gdb, @g32)
    assert MapSet.new([g32, g32perm, g41]) == g32homeos
  end

  @tag timeout: 60_000
  test "all graphs" do
    # OEIS A000088
    # n:   2 3  4  5   6    7     8      9
    # gdb: 2 4 11 34 156 1044 12346 274668

    # do_gdb(3, false, 4, 4)  |>  IO.inspect()
    # do_gdb(4, false, 11, 11)
    do_gdb(5, false, 34, 32)

    # OEIS A001349
    # n:   2 3 4  5   6   7     8      9
    # gdb: 1 2 6 21 112 853 11117 261080

    # do_gdb(3, true, 2, 2) 
    # do_gdb(4, true, 6, 6)
    _gdb5 = do_gdb(5, true, 21, 19)

    # Gdb.query_isomorphic(gdb5, @g32) |> IO.inspect(label: "isomorf g32")

    # Gdb.query_homeomorphic(gdb5, @g32) |> IO.inspect(label: "homeomorf g32")

    # Gdb.query_isomorphic(gdb5, @g41) |> IO.inspect(label: "isomorf g41")

    # Gdb.query_homeomorphic(gdb5, @g41) |> IO.inspect(label: "homeomorf g41")

    _gdb6 = do_gdb(6, true, 112, 87)
  end

  defp do_gdb(n, connected?, ng, nh) do
    {:gdb, _, homeos, contras} = gdb = Build.all_graphs(n, connected?)
    IO.inspect(Gdb.ngraph(gdb), label: "n #{n} graphs")
    IO.inspect(Gdb.niso(gdb), label: "n #{n} isos")
    IO.inspect(Gdb.nhomeo(gdb), label: "n #{n} homeos")

    assert ng == Gdb.niso(gdb)
    assert nh == Gdb.nhomeo(gdb)

    Enum.each(homeos, fn
      {hk, hs} when length(hs) > 1 ->
        IO.inspect(length(hs))

        names =
          hs
          |> Enum.flat_map(fn h -> Map.fetch!(contras, h) end)
          |> Enum.map(&Graf.name/1)

        IO.inspect(hk, label: "#{names}")

      _ ->
        :ok
    end)

    # draw all connected graphs
    # name using the undirected degree histogram
    if connected? do
      out_dir = Path.join(@out_dir, "#{n}")

      gattrs = %{
        :node => [label: "", shape: :circle, width: 0.1, fillcolor: :lightgray, style: :filled],
        :edge => [color: :gray]
      }

      opts = [edge_pair: :none]

      Enum.map(Gdb.isomorphisms(gdb), fn g ->
        histo = Morf.degree_histo1d(g, :in_out)
        hlist = Histo1D.to_list(histo)
        uhist = validate_histo(hlist, n)
        [all, nstr, code] = String.split(Graf.name(g), "_")
        name = Enum.join([all, nstr, uhist, code], "_")
        g |> Graf.rename(name) |> Draw.graph(out_dir, gattrs, :png, opts)
        name
      end)
    end

    gdb
  end

  defp validate_histo(hlist, n) do
    # histo list values are node counts
    # so sum of histo values is n
    assert Enum.sum(hlist) == n
    # degree is the 0-based array index in the histogram
    # so the maximum length of the list is 1 + 2*(n-1)
    # when at least one node is linked to all other nodes
    assert length(hlist) <= 1 + 2 * (n - 1)

    # split the histo into zeroth, odds and evens:
    [d0 | evens] = Enum.take_every(hlist, 2)
    odds = Enum.take_every(tl(hlist), 2)

    # the graph is connected, so the degree is never 0
    assert d0 == 0
    # bidirectional edges mean degree is always even
    # so all odd indexes will also be 0
    assert Enum.all?(odds, &(&1 == 0))

    # evens is the undirected degree histo
    # pad with zeroes up to the max length
    Enum.join(evens ++ List.duplicate(0, n - length(evens) - 1))
  end
end
