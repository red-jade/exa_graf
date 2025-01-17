defmodule Exa.Graf.Gdb do
  @moduledoc """
  Graph database for isomomorphism/homeomorphism query.
  """
  require Logger

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

  @doc "Add a graph to the db."
  @spec add(gdb(), G.graph()) :: gdb()
  def add(gdb, g) when is_graph(g) do
    Mol.append(gdb, Graf.gkey(g), g)
  end

  @doc """
  Query the database to find isomorphic graphs.
  """
  @spec query_isomorphic(gdb(), G.graph()) :: Mol.mol(:isomorphic | :undecided, G.graph())
  def query_isomorphic(gdb, g) when is_graph(g) do
    # TODO - filter into two sets
    gkey = Graf.gkey(g) |> IO.inspect(label: "query key")

    gdb
    |> Mol.get(gkey)
    |> IO.inspect(label: "mol")
    |> Enum.group_by(fn m ->
      case Graf.isomorphism(m, g) do
        {:isomorphic, _mapping} -> :isomorphic
        :not_isomorphic -> :not_isomorphic
        :undecided -> :undecided
      end
    end)
    |> Map.delete(:not_isomorphic)
    |> IO.inspect()
  end
end
