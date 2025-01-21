defmodule Exa.Graf.Gdb do
  @moduledoc """
  Graph database for isomomorphism/homeomorphism query.
  """
  require Logger

  import Exa.Types

  use Exa.Graf.Constants
  import Exa.Graf.Types

  # alias Exa.Types, as: E
  alias Exa.Graf.Types, as: G

  alias Exa.Std.Mol
  alias Exa.Graf.Graf

  # -----
  # types
  # -----

  # TODO
  # could speed up query by storing partial indices, not just key
  # see Graf.isomorphism for details

  @typedoc "GDB database index."
  @type gdb() :: Mol.mol(G.gkey(), G.graph())

  # ----------------
  # public functions
  # ----------------

  @doc "Create a new empty graph db."
  @spec new() :: gdb()
  def new(), do: Mol.new()

  @doc "Add a graph to the GDB."
  @spec add(gdb(), G.graph()) :: gdb()
  def add(gdb, g) when is_graph(g), do: do_add(gdb, Graf.gkey(g), g)

  @spec do_add(gdb(), G.gkey(), G.graph()) :: gdb()
  defp do_add(gdb, gkey, g), do: Mol.append(gdb, gkey, g)

  @doc """
  Add a graph to the GDB,
  but only if the graph is unique 
  (i.e. not isomorphic to any existing graph).

  Undecided graphs are added, although they may not be unique.
  """
  @spec add_unique(gdb(), G.graph()) :: gdb()
  def add_unique(gdb, g) when is_graph(g) do
    gkey = Graf.gkey(g)

    if gdb |> Mol.get(gkey) |> Enum.any?(fn h -> isomorphic?(g,h) end) do
      gdb
    else
      do_add(gdb, gkey, g)
    end
  end

  @spec isomorphic?(G.graph(), G.graph()) :: bool()
  defp isomorphic?(g1, g2), do: g1 |> Graf.isomorphism(g2) |> is_tuple_tag(:isomorphic)

  @doc """
  Query the database to find isomorphic graphs.
  """
  @spec query_isomorphic(gdb(), G.graph()) :: Mol.mol(:isomorphic | :undecided, G.graph())
  def query_isomorphic(gdb, g) when is_graph(g) do
    gkey = Graf.gkey(g)

    gdb
    |> Mol.get(gkey)
    |> Enum.group_by(fn m ->
      case Graf.isomorphism(m, g) do
        {:isomorphic, _mapping} -> :isomorphic
        :not_isomorphic -> :not_isomorphic
        :undecided -> :undecided
      end
    end)
    |> Map.delete(:not_isomorphic)
  end
end
