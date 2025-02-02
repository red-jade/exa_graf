defmodule Exa.Graf.Gdb do
  @moduledoc """
  Graph database for isomomorphism/homeomorphism query.
  """
  require Logger

  import Exa.Types

  import Exa.Std.Mol
  alias Exa.Std.Mol

  use Exa.Graf.Constants
  import Exa.Graf.Types

  # alias Exa.Types, as: E
  alias Exa.Graf.Types, as: G

  alias Exa.Graf.Graf

  # -----
  # types
  # -----

  defmodule T do
    alias Exa.Std.Mol

    # TODO
    # could speed up query by storing partial indices, not just key
    # see Graf.isomorphism for details

    @typedoc """
    Index for graph isomorphism.

    A map of structural graph hashes to a list of matching graphs.
    """
    @type iso_index() :: Mol.mol(G.gkey(), G.graph())

    defguard is_iso(iso) when is_mol(iso)

    @typedoc """
    Index for graph contraction.

    A map of a contracted graph to a list 
    of the full graphs that have the contraction.
    """
    @type con_index() :: Mol.mol(G.graph(), G.graph())

    @typedoc """
    GDB database index.

    The three indexes are:
    - isomorphism 
    - homeomorphism (topological equivalence)
    - graph-contraction correspondence
    """
    @type gdb() :: {
            :gdb,
            isomorphism :: iso_index(),
            homeomorphism :: iso_index(),
            contraction :: con_index()
          }

    defguard is_gdb(gdb) when is_tuple_tag(gdb, 4, :gdb)
  end

  # ------------
  # local module
  # ------------

  defmodule IsoIndex do
    require Exa.Graf.Gdb.T
    import  Exa.Graf.Gdb.T

    @doc "Create a new empty iso index."
    @spec new() :: T.iso_index()
    def new(), do: Mol.new()

    @doc "Add a graph to the iso index."
    @spec add(T.iso_index(), G.graph()) :: T.iso_index()
    def add(iso, g) when is_iso(iso) and is_graph(g), do: do_add(iso, Graf.gkey(g), g)

    @spec do_add(T.iso_index(), G.gkey(), G.graph()) :: T.iso_index()
    defp do_add(iso, gkey, g), do: Mol.append(iso, gkey, g)

    @doc """
    Add a graph to the iso index,
    but only if the graph is unique 
    (i.e. not isomorphic to any existing graph).

    Undecided graphs are added, although they may not be unique.
    """
    @spec add_unique(T.iso_index(), G.graph()) :: T.iso_index()
    def add_unique(iso, g) when is_iso(iso) and is_graph(g) do
      gkey = Graf.gkey(g)

      if iso |> Mol.get(gkey) |> Enum.any?(fn h -> isomorphic?(g, h) end) do
        iso
      else
        do_add(iso, gkey, g)
      end
    end

    @spec isomorphic?(G.graph(), G.graph()) :: bool()
    defp isomorphic?(g1, g2), do: g1 |> Graf.isomorphism(g2) |> is_tuple_tag(:isomorphic)

    @doc """
    Get the number of graphs.

    The number of graphs is the total number of values in the index.

    If all graphs have been added uniquely (up to isomorphism)
    then the number of graphs will be equal to the number of isomorphism classes.
    """
    @spec ngraph(T.iso_index()) :: E.count()
    def ngraph(iso) when is_iso(iso), do: Mol.lengths(iso)

    @doc """
    Get the number of isomorphism equivalence classes.

    The number of iso classes is just the number of entries (keys) in the index.
    """
    @spec nclass(T.iso_index()) :: E.count()
    def nclass(iso) when is_iso(iso), do: map_size(iso)

    @doc """
    Query the index to find isomorphic graphs.
    """
    @spec query(T.iso_index(), G.graph()) :: Mol.mol(:isomorphic | :undecided, G.graph())
    def query(iso, g) when is_iso(iso) and is_graph(g) do
      gkey = Graf.gkey(g)

      iso
      |> Mol.get(gkey)
      |> Enum.group_by(fn m ->
        case Graf.isomorphism(m, g) do
          :not_isomorphic -> :not_isomorphic
          :undecided -> :undecided
          {:isomorphic, _mapping} -> :isomorphic
        end
      end)
      |> Map.delete(:not_isomorphic)
    end
  end

  # ----------------
  # public functions
  # ----------------

  @doc "Create a new empty GDB store."
  @spec new() :: T.gdb()
  def new() do
    {:gdb, IsoIndex.new(), IsoIndex.new(), Mol.new()}
  end

  @doc "Add a graph to the GDB store."
  @spec add(T.gdb(), G.graph()) :: T.gdb()
  def add({:gdb, isos, homeos, contras}, g) when is_graph(g) do
    con = Graf.contract_linears(g)

    # all graphs are added, not just those unique up to isomorphism
    {
      :gdb,
      IsoIndex.add(isos, g),
      IsoIndex.add(homeos, con),
      Mol.add(contras, con, g)
    }
  end

  @doc """
  Get the number of graphs in the GDB store.

  The number of graphs is just the total number of 
  values in the isomorphism index.
  """
  @spec ngraph(T.gdb()) :: E.count()
  def ngraph({:gdb, isos, _homeos, _contras}), do: IsoIndex.ngraph(isos)

  @doc """
  Get the number of isomorphism equivalence classes in the GDB store.

  The number of iso classes is just the 
  number of entries in the isomorphism index.
  """
  @spec niso(T.gdb()) :: E.count()
  def niso({:gdb, isos, _homeos, _contras}), do: IsoIndex.nclass(isos)

  @doc """
  Get the number of homeomorphism equivalence classes in the GDB store.

  The number of homeo classes is just the 
  number of entries in the homeomorphism index.
  """
  @spec nhomeo(T.gdb()) :: E.count()
  def nhomeo({:gdb, _isos, homeos, _contras}), do: IsoIndex.nclass(homeos)

  @doc """
  Query the GDB store for isomorphic graphs.

  The result has two classes of matches: 
  `:isomorphic` and `:undecided` (timeout).
  """
  @spec query_isomorphic(T.gdb(), G.graph()) :: Mol.mol(:isomorphic | :undecided, G.graph())
  def query_isomorphic({:gdb, isos, _homeos, _contras}, g) when is_graph(g) do
    IsoIndex.query(isos, g)
  end

  @doc """
  Query the GDB store for homeomorphic graphs.

  The result has two classes of matches: 
  `:homeomorphic` and `:undecided` (timeout).
  """
  @spec query_homeomorphic(T.gdb(), G.graph()) :: Mol.mol(:homeomorphic | :undecided, G.graph())
  def query_homeomorphic({:gdb, _isos, homeos, contras}, g) when is_graph(g) do
    con = Graf.contract_linears(g)

    homeos
    |> IsoIndex.query(con)
    |> Enum.reduce(Mol.new(), fn {key, cs}, out ->
      key = case key do
              :isomorphic -> :homeomorphic
              :undecided -> :undecided
            end
      gs = Enum.flat_map(cs, fn c -> Map.fetch!(contras, c) end)
      Mol.prepends(out, key, gs)
    end)
  end
end
