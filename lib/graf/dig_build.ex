defmodule Exa.Graf.DigBuild do
  @moduledoc """
  Utilities for building directed graphs using the Erlang _digraph_ library.
  """

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Random

  alias Exa.Graf.Types, as: G

  alias Exa.Graf.Dig

  # ------------
  # constructors
  # ------------

  @doc "Disconnected dust topology."
  @spec dust(E.count1()) :: G.dig()
  def dust(n) when is_count1(n), do: Dig.new("dust_#{n}", [1..n])

  @doc "Simple directed line topology."
  @spec line(E.count1()) :: G.dig()
  def line(n) when is_count1(n) do
    g = "line_#{n}" |> Dig.new() |> Dig.add(1)
    Enum.reduce(2..n, g, fn i, g -> Dig.add(g,{i - 1, i}) end)
  end

  @doc "Simple ring topology."
  @spec ring(E.count1()) :: G.dig()
  def ring(n) when is_count1(n) do
    n |> line() |> Dig.add({n, 1}) |> Dig.rename("ring_#{n}")
  end

  @doc "Star topology directed outwards."
  @spec fan_out(E.count1()) :: G.dig()
  def fan_out(n) when is_integer(n) and n > 2 do
    g = "fan_out_#{n}" |> Dig.new() |> Dig.add(1)
    Enum.reduce(2..n, g, fn i, g -> Dig.add(g, {1, i}) end)
  end

  @doc "Star topology directed inwards."
  @spec fan_in(E.count1()) :: G.dig()
  def fan_in(n) when is_integer(n) and n > 2 do
    g = "fan_in_#{n}" |> Dig.new() |> Dig.add(1)
    Enum.reduce(2..n, g, fn i, g -> Dig.add(g, {i, 1}) end)
  end

  @doc "Wheel topology directed outwards."
  @spec wheel(E.count1()) :: G.dig()
  def wheel(n) when is_integer(n) and n > 3 do
    g = n |> fan_out() |> Dig.add({n, 2}) |> Dig.rename("wheel_#{n}")
    Enum.reduce(3..n, g, fn i, g -> Dig.add(g, {i - 1, i}) end)
  end

  @doc "Clique fully connected in both directions."
  @spec clique(E.count1()) :: G.dig()
  def clique(n) when is_count1(n) do
    g = "clique_#{n}" |> Dig.new() |> Dig.add(1..n)

    edges =
      for i <- 1..n do
        for j <- 1..n, j != i, do: {i, j}
      end

    Dig.add(g, edges)
  end

  @doc "Create a regular 2D lattice with undirected (bidirectional) edges."
  @spec grid2d(E.count1(), E.count1()) :: G.dig()
  def grid2d(nx, ny) when is_count1(nx) and is_count1(ny) do
    g = "grid_#{nx}_#{ny}" |> Dig.new() |> Dig.add(1..(nx * ny))

    Enum.reduce(1..ny, 1, fn _y, u ->
      Enum.reduce(1..(nx - 1), u, fn _x, u ->
        v = u + 1
        Dig.add(g, [{u, v}, {v, u}])
        v
      end) + 1
    end)

    Enum.reduce(1..(ny - 1), 1, fn _y, u ->
      Enum.reduce(1..nx, u, fn _x, u ->
        v = u + nx
        Dig.add(g, [{u, v}, {v, u}])
        u + 1
      end)
    end)

    g
  end

  @doc """
  Random graph.

  The number of edges, m, must be greater 
  than the number of vertices, n, for the graph to be connected: m > n.

  The number of edges should not be close to a complete graph: m << n^2
  """
  @spec random(E.count1(), E.count1(), bool()) :: G.dig()
  def random(n, m, force_connected \\ true)
      when is_count1(n) and
             is_integer(m) and m > n and m < 2 * n * (n - 1) do
    g = Dig.new("random_#{n}_#{m}", [1..n])
    :ok = random_edges(g, n, m, force_connected)
    g
  end

  # -----------------
  # private functions
  # -----------------

  # TODO - random construction is a quick hack ...

  # Create M random edges avoiding 
  # self-loops and repeated connections.
  # Will be very inefficiient approaching a complete graph m -> n(n-1)/2,
  # so best when m << n^2
  @spec random_edges(G.dig(), E.count1(), E.count1(), bool()) :: :ok
  defp random_edges(g, n, m, force_connected)

  # TODO - expand the bool to be an atom of connection choices:
  # connected (1 conn comp)
  # not connected (1+ conn comp)
  # k components  (k <= n)

  defp random_edges(g, n, m, false), do: erandom(g, n, m)

  defp random_edges(g, n, m, true) do
    # force every vertex to have at least one edge
    # to ensure that the whole graph is (weakly) connected
    Enum.each(1..n, fn i -> Dig.add(g, {i, Random.rndint(n, i)}) end)

    # add remaining edges randomly
    erandom(g, n, m - n)
  end

  @spec erandom(G.dig(), E.count1(), E.count()) :: :ok

  defp erandom(g, n, 0) do
    # assert result   
    ^n = Dig.nvert(g)
    :ok
  end

  defp erandom(g, n, k) do
    i = Random.rndint(n)
    j = Random.rndint(n, i)

    if Dig.edge?(g, {i, j}) do
      erandom(g, n, k)
    else
      Dig.add(g, {i, j})
      erandom(g, n, k - 1)
    end
  end
end
