defmodule Exa.Graf.GrafBuild do
  @moduledoc """
  Utilities for building directed graphs.
  """
  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Random

  alias Exa.Graf.Types, as: G
  alias Exa.Graf.Graf

  # ------------
  # constructors
  # ------------

  @doc "Disconnected dust topology."
  @spec dust(G.gtype(), E.count1()) :: G.graph()
  def dust(t, n) when is_count1(n), do: Graf.build(t, "dust_#{n}", [1..n])

  @doc "Simple directed line topology."
  @spec line(G.gtype(), E.count1()) :: G.graph()
  def line(t, n) when is_count1(n) do
    Graf.build(
      t,
      "line_#{n}",
      Enum.reduce(2..n, [], fn i, g -> [{i - 1, i} | g] end)
    )
  end

  @doc "Simple ring topology."
  @spec ring(G.gtype(), E.count1()) :: G.graph()
  def ring(t, n) when is_count1(n) do
    Graf.build(
      t,
      "ring_#{n}",
      Enum.reduce(2..n, [{n, 1}], fn i, g -> [{i - 1, i} | g] end)
    )
  end

  @doc "Star topology directed outwards."
  @spec fan_out(G.gtype(), E.count1()) :: G.graph()
  def fan_out(t, n) when is_integer(n) and n > 2 do
    Graf.build(t, "fan_out_#{n}", Enum.reduce(2..n, [], fn i, g -> [{1, i} | g] end))
  end

  @doc "Star topology directed inwards."
  @spec fan_in(G.gtype(), E.count1()) :: G.graph()
  def fan_in(t, n) when is_integer(n) and n > 2 do
    Graf.build(
      t,
      "fan_in_#{n}",
      Enum.reduce(2..n, [], fn i, g -> [{i, 1} | g] end)
    )
  end

  @doc "Wheel topology directed outwards."
  @spec wheel(G.gtype(), E.count1()) :: G.graph()
  def wheel(t, n) when is_integer(n) and n > 3 do
    Graf.build(
      t,
      "wheel_#{n}",
      Enum.reduce(2..n, [{n, 2}], fn i, g ->
        [{1, i}, {i - 1, i} | g]
      end)
    )
  end

  @doc "Clique fully connected in both directions."
  @spec clique(G.gtype(), E.count1()) :: G.graph()
  def clique(t, n) when is_count1(n) do
    Graf.build(
      t,
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
  @spec grid2d(G.gtype(), E.count1(), E.count1()) :: G.graph()
  def grid2d(t, nx, ny) when is_count1(nx) and is_count1(ny) do
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

    Graf.build(t, "grid_#{nx}_#{ny}", es)
  end

  @doc """
  Random graph.

  No self-loops.

  The number of edges, m, must be greater 
  than the number of vertices, n, for the graph to be connected: `m > n`

  The number of edges should not be close to a complete graph: `m << n^2`
  """
  @spec random(G.gtype(), E.count1(), E.count1(), bool()) :: G.graph()
  def random(t, n, m, connected? \\ true)
      when is_integer(n) and n > 1 and
             is_integer(m) and m > n and
             m < 2 * n * (n - 1) do
    Graf.new(t, "random_#{n}_#{m}") |> do_random(n, m, connected?)
  end

  # -----------------
  # private functions
  # -----------------

  # TODO - random construction is a quick hack ...

  # Create M random edges avoiding 
  # self-loops and repeated connections.
  # Will be very inefficiient approaching a complete graph m -> n(n-1)/2,
  # so best when m << n^2
  @spec do_random(G.graph(), E.count1(), E.count1(), bool()) :: G.graph()
  defp do_random(g, n, m, connected?)

  # TODO - expand the bool to be an atom of connection choices:
  # connected (1 conn comp)
  # not connected (1+ conn comp)
  # k components  (k <= n)

  # declare all vertices, because they may be isolated
  defp do_random(g, n, m, false) do
    g |> erandom(n, m) |> Graf.add([1..n])
  end

  defp do_random(g, n, m, true) do
    # force every vertex to have at least one edge
    # ensuring that the whole graph is (weakly) connected
    # no need to declare vertices, because they all occur in edges
    1..n
    |> Enum.reduce(g, fn i, g ->
      Graf.add(g, {i, Random.rndint(n, i)})
    end)
    |> erandom(n, m - n)
  end

  @spec erandom(G.graph(), E.count1(), E.count()) :: G.graph()

  defp erandom(g, _n, 0), do: g

  defp erandom(g, n, k) do
    i = Random.rndint(n)
    j = Random.rndint(n, i)

    if Graf.edge?(g, {i, j}) do
      erandom(g, n, k)
    else
      g |> Graf.add({i, j}) |> erandom(n, k - 1)
    end
  end
end
