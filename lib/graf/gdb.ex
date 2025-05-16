defmodule Exa.Graf.Gdb do
  @moduledoc """
  Graph database for isomomorphism/homeomorphism query.
  """
  require Logger

  import Exa.Types

  alias Exa.Std.Mos

  use Exa.Graf.Constants

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Graf.Morf, as: M
  alias Exa.Graf.Morf

  alias Exa.Graf.Contract

  # -----
  # types
  # -----

  defmodule T do
    alias Exa.Std.Mos

    @typedoc """
    Index for graph isomorphism.

    A map of structural graph hash to a map
    of exemplar graph to a set of isomorphic/undecided graphs.
    """
    @type iso_index() :: %{G.gkey() => Mos.mos(G.graph(), G.graph())}

    defguard is_iso(iso) when is_map(iso)

    @typedoc """
    Index for graph contraction.

    A map of a contracted graph to a set of
    the full graphs that have the contraction.
    """
    @type con_index() :: Mos.mos(G.graph(), G.graph())

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
    import Exa.Graf.Gdb.T

    @doc "Create a new empty iso index."
    @spec new() :: T.iso_index()
    def new(), do: Map.new()

    @doc "Add a graph to the iso index."
    @spec add(T.iso_index(), G.graph()) :: T.iso_index()
    def add(iso, g) when is_iso(iso) and is_graph(g), do: do_add(iso, Morf.gkey(g), g)

    @spec do_add(T.iso_index(), M.gkey(), G.graph()) :: T.iso_index()

    defp do_add(iso, gkey, g) when is_map_key(iso, gkey) do
      gmos = Map.fetch!(iso, gkey)
      exs = Map.keys(gmos)

      # graph is added to every exemplar key 
      # that is isomorphic or undecided status
      {gmos, updated?} =
        Enum.reduce(exs, {gmos, false}, fn ex, {gmos, _updated?} = acc ->
          case Morf.isomorphism(g, ex) do
            :not_isomorphic -> acc
            _iso_or_undecided -> {Mos.add(gmos, ex, g), true}
          end
        end)

      # if no existing match create new exemplar
      gmos = if updated?, do: gmos, else: Mos.add(gmos, g, g)

      Map.put(iso, gkey, gmos)
    end

    defp do_add(iso, gkey, g) do
      # new gkey iso set with identity mapping
      gmos = Mos.new() |> Mos.add(g, g)
      Map.put(iso, gkey, gmos)
    end

    @doc "Get all the graphs in the index."
    @spec graphs(T.iso_index()) :: MapSet.t(G.graph())
    def graphs(iso) when is_iso(iso) do
      # index values are distinct, graphs may be repeated
      # so set semantics are necessary
      Enum.reduce(Map.values(iso), MapSet.new(), fn gmos, gs ->
        gmos |> Mos.union_values() |> MapSet.union(gs)
      end)
    end

    @doc """
    Get the number of graphs.

    The number of graphs is the total number of 
    distinct values in the index.
    """
    @spec ngraph(T.iso_index()) :: E.count()
    def ngraph(iso) when is_iso(iso) do
      Enum.reduce(Map.values(iso), 0, fn gmos, n ->
        n + (gmos |> Mos.union_values() |> MapSet.size())
      end)
    end

    @doc """
    Get all isomorphism equivalence classes.

    Each isomorphism class is represented by an exemplar,
    which has a specific labelling. 
    """
    @spec classes(T.iso_index()) :: MapSet.t(G.graph())
    def classes(iso) when is_iso(iso) do
      # index keys are all distinct
      # so set semantics are not strictly necessary
      Enum.reduce(Map.values(iso), MapSet.new(), fn gmos, exs ->
        gmos |> Map.keys() |> MapSet.new() |> MapSet.union(exs)
      end)
    end

    @doc """
    Get the number of isomorphism equivalence classes.

    The number of iso classes is the number of exemplars in the index.
    It is calculated as the sum of entries over all graph keys.
    """
    @spec nclass(T.iso_index()) :: E.count()
    def nclass(iso) when is_iso(iso) do
      Enum.reduce(Map.values(iso), 0, fn gmos, n ->
        n + map_size(gmos)
      end)
    end

    @doc """
    Query the index to find isomorphic graphs.
    """
    @spec query(T.iso_index(), G.graph()) :: MapSet.t(G.graph())
    def query(iso, g) when is_iso(iso) and is_graph(g) do
      gkey = Morf.gkey(g)
      gset = MapSet.new()

      case Map.get(iso, gkey) do
        nil ->
          gset

        gmos ->
          # accumulate all graphs for every exemplar key 
          # that is isomorphic or undecided status
          Enum.reduce(Map.keys(gmos), gset, fn ex, gset ->
            case Morf.isomorphism(g, ex) do
              :not_isomorphic -> gset
              _iso_or_undecided -> MapSet.union(gset, Mos.get(gmos, ex))
            end
          end)
      end
    end
  end

  # ----------------
  # public functions
  # ----------------

  @doc "Create a new empty GDB store."
  @spec new() :: T.gdb()
  def new() do
    {:gdb, IsoIndex.new(), IsoIndex.new(), Mos.new()}
  end

  @doc "Add a graph to the GDB store."
  @spec add(T.gdb(), G.graph()) :: T.gdb()
  def add({:gdb, isos, homeos, contras}, g) when is_graph(g) do
    con = Contract.linears(g)

    # all graphs are added, not just those unique up to isomorphism
    {
      :gdb,
      IsoIndex.add(isos, g),
      IsoIndex.add(homeos, con),
      Mos.add(contras, con, g)
    }
  end

  @doc "Get all the graphs in the GDB store."
  @spec graphs(T.gdb()) :: MapSet.t(G.graph())
  def graphs({:gdb, isos, _homeos, _contras}), do: IsoIndex.graphs(isos)

  @doc """
  Get the number of graphs in the GDB store.

  The number of graphs is just the total number of 
  values in the isomorphism index.
  """
  @spec ngraph(T.gdb()) :: E.count()
  def ngraph({:gdb, isos, _homeos, _contras}), do: IsoIndex.ngraph(isos)

  @doc "Get exemplars for all isomorphism equivalence classes in the GDB store."
  @spec isomorphisms(T.gdb()) :: MapSet.t(G.graph())
  def isomorphisms({:gdb, isos, _homeos, _contras}), do: IsoIndex.classes(isos)

  @doc """
  Get the number of isomorphism equivalence classes in the GDB store.

  The number of iso classes is just the 
  number of entries in the isomorphism index.

  The return value will be less than or equal to the true value,
  because one hash index will contain more than one equivalence class,
  if the full graph hash is not enough to discriminate an isomorphism.
  """
  @spec niso(T.gdb()) :: E.count()
  def niso({:gdb, isos, _homeos, _contras}), do: IsoIndex.nclass(isos)

  @doc "Get exemplars for all homeomorphism equivalence classes in the GDB store."
  @spec homeomorphisms(T.gdb()) :: MapSet.t(G.graph())
  def homeomorphisms({:gdb, _isos, homeos, _contras}), do: IsoIndex.classes(homeos)

  @doc """
  Get the number of homeomorphism equivalence classes in the GDB store.

  The number of homeo classes is just the 
  number of entries in the homeomorphism index.

  The return value will be less than or equal to the true value,
  because one hash index will contain more than one equivalence class,
  if the full graph hash is not enough to discriminate an isomorphism.
  """
  @spec nhomeo(T.gdb()) :: E.count()
  def nhomeo({:gdb, _isos, homeos, _contras}), do: IsoIndex.nclass(homeos)

  @doc """
  Query the GDB store for isomorphic graphs.

  The result has includes isomorphic and undecided (timeout) matches.
  """
  @spec query_isomorphic(T.gdb(), G.graph()) :: MapSet.t(G.graph())
  def query_isomorphic({:gdb, isos, _homeos, _contras}, g) when is_graph(g) do
    IsoIndex.query(isos, g)
  end

  @doc """
  Query the GDB store for homeomorphic graphs.

  The result has includes isomorphic and undecided (timeout) matches.
  """
  @spec query_homeomorphic(T.gdb(), G.graph()) :: MapSet.t(G.graph())
  def query_homeomorphic({:gdb, _isos, homeos, contras}, g) when is_graph(g) do
    con = Contract.linears(g)

    homeos
    |> IsoIndex.query(con)
    |> Enum.reduce(MapSet.new(), fn c, out ->
      contras |> Mos.get(c) |> MapSet.union(out)
    end)
  end
end
