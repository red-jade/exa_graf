defmodule Exa.Graf.AgraBuild do
  @moduledoc """
  Utilities for building directed graphs in agra format.
  """

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Random
  alias Exa.Std.Mos

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G
  alias Exa.Graf.Agra

  # ------------
  # constructors
  # ------------

  @doc "Disconnected dust topology."
  @spec dust(E.count1()) :: G.agra()
  def dust(n) when is_count1(n), do: Agra.new("dust_#{n}", [1..n])

  @doc "Simple directed line topology."
  @spec line(E.count1()) :: G.agra()
  def line(n) when is_count1(n) do
    Agra.new(
      "line_#{n}",
      Enum.reduce(2..n, [], fn i, g -> [{i - 1, i} | g] end)
    )
  end

  @doc "Simple ring topology."
  @spec ring(E.count1()) :: G.agra()
  def ring(n) when is_count1(n) do
    Agra.new(
      "ring_#{n}",
      Enum.reduce(2..n, [{n, 1}], fn i, g -> [{i - 1, i} | g] end)
    )
  end

  @doc "Star topology directed outwards."
  @spec fan_out(E.count1()) :: G.agra()
  def fan_out(n) when is_integer(n) and n > 2 do
    Agra.new("fan_out_#{n}", Enum.reduce(2..n, [], fn i, g -> [{1, i} | g] end))
  end

  @doc "Star topology directed inwards."
  @spec fan_in(E.count1()) :: G.agra()
  def fan_in(n) when is_integer(n) and n > 2 do
    Agra.new(
      "fan_in_#{n}",
      Enum.reduce(2..n, [], fn i, g -> [{i, 1} | g] end)
    )
  end

  @doc "Wheel topology directed outwards."
  @spec wheel(E.count1()) :: G.agra()
  def wheel(n) when is_integer(n) and n > 3 do
    Agra.new(
      "wheel_#{n}",
      Enum.reduce(2..n, [{n, 2}], fn i, g ->
        [{1, i}, {i - 1, i} | g]
      end)
    )
  end

  @doc "Clique fully connected in both directions."
  @spec clique(E.count1()) :: G.agra()
  def clique(n) when is_count1(n) do
    Agra.new(
      "clique_#{n}",
      Enum.reduce(1..n, [], fn i, g ->
        Enum.reduce(1..n, g, fn
          j, g when i != j -> [{i, j} | g]
          _, g -> g
        end)
      end)
    )
  end

  @doc "Create a regular 2D lattice with undirected (bidirectional) edges."
  @spec grid2d(E.count1(), E.count1()) :: G.agra()
  def grid2d(nx, ny) when is_count1(nx) and is_count1(ny) do
    # join rows
    {_, es} =
      Enum.reduce(1..ny, {1, []}, fn _j, acc ->
        {u, es} =
          Enum.reduce(1..(nx - 1), acc, fn _i, {u, es} ->
            v = u + 1
            {v, [{u, v}, {v, u} | es]}
          end)

        {u + 1, es}
      end)

    # join columns
    {_, es} =
      Enum.reduce(1..(ny - 1), {1, es}, fn _j, acc ->
        Enum.reduce(1..nx, acc, fn
          _i, {u, es} ->
            v = u + nx
            {u + 1, [{u, v}, {v, u} | es]}
        end)
      end)

    Agra.new("grid_#{nx}_#{ny}", es)
  end

  @doc """
  Random graph.

  No self-loops.

  The number of edges, m, must be greater 
  than the number of vertices, n, for the graph to be connected: `m > n`

  The number of edges should not be close to a complete graph: `m << n^2`
  """
  @spec random(E.count1(), E.count1(), bool()) :: G.agra()
  def random(n, m, connected? \\ true)
      when is_integer(n) and n > 1 and
             is_integer(m) and m > n and
             m < 2 * n * (n - 1) do
    {:agra, "random_#{n}_#{m}", do_random(n, m, connected?)}
  end

  # -----------------
  # private functions
  # -----------------

  # TODO - random construction is a quick hack ...

  # Create M random edges avoiding 
  # self-loops and repeated connections.
  # Will be very inefficiient approaching a complete graph m -> n(n-1)/2,
  # so best when m << n^2
  @spec do_random(n :: E.count1(), m :: E.count1(), connected? :: bool()) :: G.adjmaps()

  # TODO - expand the bool to be an atom of connection choices:
  # connected (1 conn comp)
  # not connected (1+ conn comp)
  # k components  (k <= n)

  # declare all vertices, because they may be isolated
  defp do_random(n, m, false) do
    {Mos.new(),Mos.new()} |> do_add(1..n) |> erandom(n, m)
  end

  defp do_random(n, m, true) do
    # force every vertex to have at least one edge
    # ensuring that the whole graph is (weakly) connected
    # no need to declare vertices, because they all occur in edges
    new_adjs =
      Enum.reduce(1..n, {Mos.new(),Mos.new()}, fn i, adjs ->
        do_add(adjs, {i, Random.rndint(n, i)})
      end)

    # add remaining edges randomly
    erandom(new_adjs, n, m - n)
  end

  @spec erandom(G.adjmaps(), E.count1(), E.count()) :: G.adjmaps()

  defp erandom(adjs, _n, 0), do: adjs

  defp erandom({_, outadj} = adjs, n, k) do
    i = Random.rndint(n)
    j = Random.rndint(n, i)

    if Mos.member?(outadj, i, j) do
      erandom(adjs, n, k)
    else
      adjs |> do_add({i, j}) |> erandom(n, k - 1)
    end
  end

  # ----------------
  # private function
  # ----------------

  # cut'n'paste from Agra so do_add can be private there

  @spec do_add(G.adjmaps(), G.edge() | G.vseq()) :: G.adjmaps()

  defp do_add({inadj, outadj}, r) when is_range(r) do
    {
      Enum.reduce(r, inadj, &Mos.touch(&2, &1) ),
      Enum.reduce(r, outadj, &Mos.touch(&2, &1) ),
    }
  end

  defp do_add({inadj, outadj}, {src, dst}) when is_vert(src) and is_vert(dst) do
    {
      inadj |> Mos.touch(src) |> Mos.add(dst, src),
      outadj |> Mos.touch(dst) |> Mos.add(src, dst)
    }
  end
end
