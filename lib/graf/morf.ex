defmodule Exa.Graf.Morf do
  @moduledoc """
  Utilities for the following graph analysis:
  - degree histograms in 1D, 2D and 3D
  - graph hashes: encode a graph as a 256-bit unsigned integer
  - isomorphisms: equality up to label permutation
  - homeomorphism: topological equivalence
  """
  require Logger

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Types
  alias Exa.Types, as: E

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Std.HistoTypes, as: H
  alias Exa.Std.Histo
  alias Exa.Std.Histo1D
  alias Exa.Std.Histo2D
  alias Exa.Std.Histo3D
  alias Exa.Std.Mol

  alias Exa.Std.Mos

  alias Exa.Graf.Graf
  alias Exa.Graf.Contract

  # ---------
  # constants
  # ---------

  # default maximum elapsed time for an isomorphism search
  @iso_timeout 20_000

  # -----
  # types
  # -----

  # global index to local in/out neighbors
  @typep neigh_index() :: %{G.vert() => G.neigh2()}

  # 0 hop local topology is just G.degree3()
  # global index of vertex id to degree3
  @typep degree_index() :: %{G.vert() => G.degree3()}

  # combine basic indexes for the graph
  @typep indexes() :: {neigh_index(), degree_index()}

  # 1 hop local topology is degree3 for the vertex,
  # plus histograms of degree3 for in/out neighbors 
  @typep local_degrees() ::
           {in_histo :: H.histo3d(), self_degree3 :: G.degree3(), out_histo :: H.histo3d()}

  # global index of vertex id to local degrees
  @typep topo_index() :: %{G.vert() => local_degrees()}

  # global index of vertex hash
  @typep hash_index() :: %{G.vert() => G.hash()}

  # global or local inverse cache of verts having a hash
  @typep hash_cache() :: Mos.mos(G.hash(), G.vert())

  # index of number of verts with a hash, to a list of hashes with that count
  @typep length_index() :: Mol.mol(E.count1(), G.hash())

  # return type for isomorphism
  @typep iso_result() :: :not_isomorphic | :undecided | {:isomorphic, G.vmap()}

  # local topology cache for vertex ins/outs having a hash
  # @typep local_hashes() :: {in_cache :: hash_cache(), out_cache :: hash_cache()}

  # local topo cache for vertex ins/outs having a hash
  # @typep topo_cache() :: %{G.vert() => local_hashes()}

  # -----------------
  # degree histograms
  # -----------------

  @doc """
  Create a 1D histogram of the vertex degrees.

  The kind of degree is determined by the adjacency argument:
  - `:in` in degree
  - `:out` out degree
  - `:in_out` total degree (in+out) including self-loops
  - `:in_self_out` total degree (in+out) ignoring self-loops

  If there are no self-loops in the graph, 
  the last two adjacency options will give the same result.

  Use `Exa.Std.Histo1D.homogeneous/1` to test for univalent graph
  (a graph where every vertex has the same degree).
  """
  @spec degree_histo1d(G.graph(), G.adjacency()) :: H.histo1d()

  def degree_histo1d(g, adjy) when adjy in [:in, :out] do
    Enum.reduce(Graf.verts(g), Histo1D.new(), fn i, h ->
      Histo1D.inc(h, Graf.degree(g, i, adjy))
    end)
  end

  def degree_histo1d(g, :in_out) do
    Enum.reduce(Graf.verts(g), Histo1D.new(), fn i, h ->
      {indeg, outdeg} = Graf.degree(g, i, :in_out)
      Histo1D.inc(h, indeg + outdeg)
    end)
  end

  def degree_histo1d(g, :in_self_out) do
    Enum.reduce(Graf.verts(g), Histo1D.new(), fn i, h ->
      {indeg, _, outdeg} = Graf.degree(g, i, :in_self_out)
      Histo1D.inc(h, indeg + outdeg)
    end)
  end

  @doc """
  Create a 2D histogram of the in and out vertex degrees.

  The kind of degree is determined by the adjacency argument:
  - `:in_out` (in,out) including self-loops
  - `:in_self_out` (in,out) ignoring self-loops

  If there are no self-loops in the graph, 
  the two adjacency options will give the same result.
  """
  @spec degree_histo2d(G.graph(), G.adjacency()) :: H.histo2d()
  def degree_histo2d(g, adjy \\ :in_out)

  def degree_histo2d(g, :in_out) do
    Enum.reduce(Graf.verts(g), Histo2D.new(), fn i, h ->
      Histo2D.inc(h, Graf.degree(g, i, :in_out))
    end)
  end

  def degree_histo2d(g, :in_self_out) do
    Enum.reduce(Graf.verts(g), Histo2D.new(), fn i, h ->
      {indeg, _, outdeg} = Graf.degree(g, i, :in_self_out)
      Histo2D.inc(h, {indeg, outdeg})
    end)
  end

  @doc "Create a 3D histogram of the (in, self, out) vertex degrees."
  @spec degree_histo3d(G.graph()) :: H.histo3d()
  def degree_histo3d(g) do
    Enum.reduce(Graf.verts(g), Histo3D.new(), fn i, h ->
      Histo3D.inc(h, Graf.degree(g, i, :in_self_out))
    end)
  end

  @doc """
  Convert the frontiers of a vertex into a histogram.

  For each frontier at hop (radius) _r_ 
  set the count to be the size of the frontier.

  The count at radius 0 is always 1 (the vertex itself).

  At a sufficiently large radius, 
  towards the maximum diameter of the graph, 
  the histogram will fall to 0.

  In between these extremes, the frontier size _F_ 
  is a proxy for the surface area of the expanding neighborhood:
  - 0 frontier means 0D (isolated vertex or component)
  - constant (non-zero) curve means 1D
  - linear curve means 2D
  - quadratic curve means 3D
  - power _p_ polynomial curve means _(p+1)D_

  It is possibe to model fractal dimensions
  and various kinds of polynomial curves 
  for different connectivities, such as  
  smooth dense edges or sparse rectangular lattices.

  For example:
  - 2D has linear histogram:
    - smooth radial gives `F = 2πr`
    - rectangular   gives `F = 8 r`

  - 3D has quadratic histogram:
    - smooth radial gives `F = 4πr²`
    - rectangular   gives `F = 24r²`
  """
  @spec frontier_histo1d(G.frontiers()) :: H.histo1d()
  def frontier_histo1d(fronts) do
    Enum.reduce(fronts, Histo1D.new(), fn {r, f}, h ->
      Histo1D.set(h, r, MapSet.size(f))
    end)
  end

  # --------------------------
  # isomorphism, homeomorphism
  # --------------------------

  @doc """
  Compare two graphs for _homeomorphism_ (topological equivalence). 

  Two graphs are _homeomorphic_ if their 
  linear contractions are isomorphic.

  Linear node contraction just reduces the count of
  nodes with 3-degree (`in_self_out`) value `{1,0,1}`.
  So two graphs are _not_ homeomorphic if their 
  3D degree histograms differ outside the `{1,0,1}` bin.

  The value will be `:undecided` if the isomorphism test times out.
  """
  @spec homeomorphic?(G.graph(), G.graph()) :: :not_homeomorphic | :undecided | :homeomorphic
  def homeomorphic?(g1, g2) when is_graph(g1) and is_graph(g2) do
    histo1 = g1 |> degree_histo3d() |> Map.delete({1, 0, 1})
    histo2 = g2 |> degree_histo3d() |> Map.delete({1, 0, 1})

    if histo1 != histo2 do
      :not_homeomorphic
    else
      gc1 = Contract.linears(g1)
      gc2 = Contract.linears(g2)

      case isomorphism(gc1, gc2) do
        :not_isomorphic -> :not_homeomorphic
        :undecided -> :undecided
        {:isomorphic, _} -> :homeomorphic
      end
    end
  end

  @doc """
  Find an isomorphism between two graphs,
  which means a structural equivalence ignoring all vertex identitiers.

  For isomorphic graphs, there is a 1-1 bijective relabelling of all vertices
  that will make the graphs exactly equal.

  If the two graphs are equal, then they are isomorphic
  with the identity (no-op) mapping.

  The result is either:
  - `:not_isomorphic`
  - `:isomorphic` with one possible vertex mapping
  - `:undecided` if the maximum time duration was exceeded

  The initial test compares:
  - number of vertices
  - number of edges
  - 0-hop hashes of the graphs 
  - 1-hop hashes of the graphs 

  Most graphs that are not isomorphic will return promptly at this stage.

  If all tests pass, then calculate an isomorphism, fail or timeout.
  """
  @spec isomorphism(G.graph(), G.graph(), E.timeout1()) :: iso_result()
  def isomorphism(g1, g2, dt \\ @iso_timeout)
      when is_graph(g1) and is_graph(g2) and is_timeout1(dt) do
    with true <- Graf.nvert(g1) == Graf.nvert(g2),
         true <- Graf.nedge(g1) == Graf.nedge(g2),
         verts1 = Graf.verts(g1),
         verts2 = Graf.verts(g2),
         {nindex1, _dindex1} = idxs1 = indexes(g1, verts1),
         {nindex2, _dindex2} = idxs2 = indexes(g2, verts2),
         # hash(g1,0) == hash(g2,0)
         g1hash0 = do_hash0(idxs1, verts1),
         g2hash0 = do_hash0(idxs2, verts2),
         true <- g1hash0 == g2hash0,
         # hash(g1,1) == hash(g2,1)
         {hindex1, g1hash1} <- do_hash1(idxs1, verts1),
         {hindex2, g2hash1} <- do_hash1(idxs2, verts2),
         true <- g1hash1 == g2hash1,
         gdata = [verts1, verts2, nindex1, nindex2, hindex1, hindex2] do
      case Exa.Exec.exec(&do_isomorphism/6, gdata) |> Exa.Exec.recv(dt) do
        {:ok, iso_or_not} -> iso_or_not
        {:timeout, _} -> :undecided
        {:error, err} -> raise(RuntimeError, message: inspect(err))
      end
    else
      false -> :not_isomorphic
    end
  end

  @spec do_isomorphism(
          G.verts(),
          G.verts(),
          neigh_index(),
          neigh_index(),
          hash_index(),
          hash_index()
        ) ::
          :not_isomorphic | {:isomorphic, G.vmap()}
  defp do_isomorphism(verts1, verts2, nindex1, nindex2, hindex1, hindex2) do
    # hash cache is MoS of hash => set of vertices with the hash
    hcache1 = hash_cache(hindex1, verts1)
    hcache2 = hash_cache(hindex2, verts2)

    # TODO - tcaches maybe useful for faster incremental solutions
    # tcache1 = topo_cache(nindex1, hindex1, verts1)
    # tcache2 = topo_cache(nindex2, hindex2, verts2)

    # length index is MoL of num vertices => list of hashes with that many vertices
    lindex1 = Mos.index_size(hcache1)
    lindex2 = Mos.index_size(hcache2)

    # lens is ascending order, but vert_lol will reverse order
    # so more ambiguous to least ambiguous (singletons)
    lengths = lindex1 |> Map.keys() |> Enum.sort()
    lol1 = vert_lol(lengths, lindex1, hcache1)
    lol2 = vert_lol(lengths, lindex2, hcache2)

    # product of all permutations over hash equivalence classes
    # for every hash with n vertices, there are n! permutations
    # the num of hashes with that length will be the power of the factorial
    Logger.info(fn -> "Isomorphism difficulty: #{Exa.Combine.nsubperms(lol1)}" end)
    morf? = fn vmap -> morf?(vmap, nindex1, nindex2) end
    submaps(lol1, lol2, morf?)
  end

  # threshold for consistency check on partial sub-mapping
  # sub-lists equal or shorter will not be pre-checked
  @small_perm 3

  # search all sub-mappings (LoM) for equivalence classes (LoL)
  @spec submaps([G.verts()], [G.verts()], E.predicate?()) :: iso_result()

  defp submaps([vl1], [vl2], morf?) do
    # all vertices in one equivalence class (e.g. Petersen)
    # just search permutations of the one list
    Logger.info(fn -> "Isomorphism candidates: #{Exa.Combine.nperms(vl1)}" end)
    # accumulator is just the number of attempted matches
    perm =
      Exa.Combine.find_permutation(vl2, 1, fn p2, n ->
        vmap = Exa.Map.zip_new(vl1, p2)
        if morf?.(vmap), do: throw({:return, {vmap, n}}), else: n + 1
      end)

    {result, n} =
      case perm do
        {:no_match, n} -> {:not_isomorphic, n}
        {:ok, {vmap, n}} -> {{:isomorphic, vmap}, n}
      end

    Logger.info(fn -> "Isomorphism attempts:   #{n}" end)
    result
  end

  defp submaps(lol1, lol2, morf?) do
    # only pre-test submaps if there's more than one non-trivial sublist
    nlong =
      Enum.reduce_while(lol1, 0, fn
        ls, 0 when length(ls) > 1 -> {:cont, 1}
        ls, 1 when length(ls) > 1 -> {:halt, 2}
        _, n -> {:cont, n}
      end)

    bigmorf? = if nlong in [0, 1], do: nil, else: morf?
    submaps = do_sub(lol1, lol2, bigmorf?, [])
    Logger.info(fn -> "Isomorphism candidates: #{Exa.Combine.nselects(submaps)}" end)

    sel =
      Exa.Combine.find_selection(submaps, 1, fn ms, n ->
        vmap = Enum.reduce(ms, &Map.merge(&1, &2))
        if morf?.(vmap), do: throw({:return, {vmap, n}}), else: n + 1
      end)

    {result, n} =
      case sel do
        {:no_match, n} -> {:not_isomorphic, n}
        {:ok, {vmap, n}} -> {{:isomorphic, vmap}, n}
      end

    Logger.info(fn -> "Isomorphism attempts:   #{n}" end)
    result
  end

  @spec do_sub([G.verts()], [G.verts()], E.predicate?(), [[G.vmap()]]) :: [[G.vmap()]]

  defp do_sub([vl1 | lol1], [vl2 | lol2], morf?, submaps) do
    subs =
      Exa.Combine.reduce_perms(vl2, [], fn p2, subs ->
        vmap = Exa.Map.zip_new(vl1, p2)

        cond do
          map_size(vmap) <= @small_perm -> [vmap | subs]
          is_nil(morf?) -> [vmap | subs]
          morf?.(vmap) -> [vmap | subs]
          true -> subs
        end
      end)

    do_sub(lol1, lol2, morf?, [subs | submaps])
  end

  defp do_sub([], [], _, submaps), do: Enum.reverse(submaps)

  # convert MoL index of length => list of hashes with that length
  # and     MoL index of hash   => list of vertices with that hash 
  # to      LoL list of ambiguous vertex sublists
  @spec vert_lol([E.count1()], length_index(), hash_cache()) :: [[G.vert()]]
  defp vert_lol(lengths, lindex, hcache) do
    Enum.reduce(lengths, [], fn len, lol ->
      Enum.reduce(lindex[len], lol, fn hash, lol ->
        [MapSet.to_list(hcache[hash]) | lol]
      end)
    end)
  end

  # test if a partial or total relabelling is consistent
  @spec morf?(G.vmap(), neigh_index(), neigh_index()) :: bool()
  defp morf?(vmap, nindex1, nindex2) do
    Enum.all?(vmap, fn {i1, i2} ->
      {ins1, outs1} = nindex1[i1]
      {ins2, outs2} = nindex2[i2]
      setmorf?(vmap, ins1, ins2) and setmorf?(vmap, outs1, outs2)
    end)
  end

  # test if total or partial relabelling is consistent for two vertex sets
  # equivalent to map and equality test, but in a single pass
  @spec setmorf?(G.vmap(), G.vset(), G.vset()) :: bool()
  defp setmorf?(vmap, vs1, vs2) do
    # could remove the map key test for total tests
    Enum.all?(vs1, fn i1 -> not is_map_key(vmap, i1) or is_set_member(vs2, vmap[i1]) end)
  end

  # ------
  # hashes
  # ------

  @doc """
  Calculate a composite hash for a graph.
  The result contains:
  - number of vertices
  - number of edges
  - 0-hop hash
  - 1-hop hash
  """
  @spec gkey(G.graph()) :: G.gkey()
  def gkey(g) when is_graph(g) do
    nvert = Graf.nvert(g)
    nedge = Graf.nedge(g)
    verts = Graf.verts(g)
    idxs = indexes(g, verts)
    hash0 = do_hash0(idxs, verts)
    {_hindex, hash1} = do_hash1(idxs, verts)
    {nvert, nedge, hash0, hash1}
  end

  @doc """
  Create a hash of the graph.

  The hash should reasonably discriminate graphs
  by their topology, with a simple and relatively fast algorithm.
  The hash ignores vertex and edge identifiers.

  The hash can be used to reject a graph isomorphism test.
  Graphs with different hashes cannot be isomorphic.
  Graphs with the same hash may be isomorphic, or not.

  The basic approach is to:
  - create a histogram from an encoding of 
    local vertex neighborhood topologies
  - convert the histogram term to binary
  - hash the binary using SHA-256
  - convert the hash to a 256-bit unsigned integer

  There are two levels of encoding the local topology of a vertex,
  based on the number of hops out from the vertex:
  - 0 hop: 3-tuple of `:in_self_out` degrees for the vertex itself
  - 1 hop: 3-tuple encoding: 
    - histogram of 3-tuple degrees for incoming neighbors 
    - vertex 3-tuple degrees
    - histogram of 3-tuple degrees for outgoing neighbors

  As the number of hops increases, 
  the work needed to calculate the hash rises dramatically.

  The hashes for different hops are not comparable.
  So hash equality tests are only valid for the same hop.

  The salting of the crypto algorithm is not stable across Erlang releases,
  or between different Erlang runtime instances,
  including successive sessions on the same machine,
  so the hash should not be distributed or persisted.
  """
  @spec hash(G.graph(), nhop :: 0 | 1) :: G.hash()

  def hash(g, 0) do
    verts = Graf.verts(g)
    g |> indexes(verts) |> do_hash0(verts)
  end

  def hash(g, 1) do
    verts = Graf.verts(g)
    g |> indexes(verts) |> do_hash1(verts) |> elem(1)
  end

  # shared hash and isomorphism utilities ----------

  @spec do_hash0(indexes(), G.verts()) :: G.hash()
  defp do_hash0({_nindex, dindex}, verts) do
    dindex |> degree_histo3d(verts) |> hash_term()
  end

  @spec do_hash1(indexes(), G.verts()) :: {hash_index(), G.hash()}
  defp do_hash1(idxs, verts) do
    hindex = hash_index(idxs, verts)
    ghash = hindex |> graph_histo(verts) |> hash_term()
    {hindex, ghash}
  end

  # build indexes of the local neighborhood and degrees
  @spec indexes(G.graph(), G.verts()) :: indexes()
  defp indexes(g, verts) do
    Enum.reduce(verts, {%{}, %{}}, fn i, {nindex, dindex} ->
      {ins, self, outs} = Graf.neighborhood(g, i, :in_self_out)
      self_deg = if is_nil(self), do: 0, else: 1
      deg3 = {MapSet.size(ins), self_deg, MapSet.size(outs)}
      {Map.put(nindex, i, {ins, outs}), Map.put(dindex, i, deg3)}
    end)
  end

  # build hash index of vertex to its hash
  # calculated from neighborhood histos of degree3 (in_self_out 3-tuples)
  @spec hash_index(indexes(), G.verts()) :: hash_index()
  defp hash_index(idxs, verts) do
    tindex = topo_index(idxs, verts)

    Enum.reduce(verts, %{}, fn i, hindex ->
      vhash = hash_term(tindex[i])
      Map.put(hindex, i, vhash)
    end)
  end

  # build topo index of vertex to local 1-hop degree histograms
  @spec topo_index(indexes(), G.verts()) :: topo_index()
  defp topo_index({nindex, dindex}, verts) do
    Enum.reduce(verts, %{}, fn i, tindex ->
      {ins, outs} = nindex[i]
      dlocal = {degree_histo3d(dindex, ins), dindex[i], degree_histo3d(dindex, outs)}
      Map.put(tindex, i, dlocal)
    end)
  end

  # build topo cache of vertex to its local topology hashes
  # @spec topo_cache(neigh_index(), hash_index(), G.verts()) :: topo_cache()
  # defp topo_cache(nindex, hindex, verts) do
  #   Enum.reduce(verts, %{}, fn i, tcache ->
  #     {ins, outs} = nindex[i]
  #     hlocal = {hash_cache(hindex, ins), hash_cache(hindex, outs)}
  #     Map.put(tcache, i, hlocal)
  #   end)
  # end

  # build a 3D histogram from indexed 3-degrees (in_self_out)
  @spec degree_histo3d(degree_index(), G.verts()) :: H.histo3d()
  defp degree_histo3d(dindex, verts) do
    Enum.reduce(verts, Histo3D.new(), fn i, h -> Histo3D.inc(h, dindex[i]) end)
  end

  # build a hash cache from indexed hashes
  # hash cache is list of verts for a given hash key
  @spec hash_cache(hash_index(), G.verts()) :: hash_cache()
  defp hash_cache(hindex, verts) do
    Enum.reduce(verts, Mos.new(), fn i, c -> Mos.add(c, hindex[i], i) end)
  end

  # build a hash histogram from indexed hashes
  @spec graph_histo(hash_index(), G.verts()) :: H.histo()
  defp graph_histo(hindex, verts) do
    Enum.reduce(verts, Histo.new(), fn i, h -> Histo.inc(h, hindex[i]) end)
  end

  # hash any term to produce a 256-bit unsigned integer
  @spec hash_term(any()) :: G.hash()
  defp hash_term(term) do
    <<i::256>> = :crypto.hash(:sha256, :erlang.term_to_binary(term, [:local]))
    i
  end
end
