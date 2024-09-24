defmodule Exa.Graf.Graf do
  @moduledoc """
  The _Graf_ module has three components:
  - define a behaviour for graph modules
  - abstract interface for graph modules
    that dispatches to the correct implementation module
  - common graph algorithms implemented 
    with the abstract interface

  The current list of graph modules is:
  - Adj: in-memory adjacency lists
  - Dig: Erlang `digraph` ETS database

  Graph data structures use tagged tuples.
  Each graph module implements a common API behaviour.
  The _Graf_ module dispatches to those modules 
  based on the tag in the tuple.
  The mechanism is like a _protocol_ for tagged tuples.
  """
  require Logger

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Types
  alias Exa.Types, as: E

  import Exa.Dispatch, only: [dispatch: 4, dispatch: 3]
  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Std.HistoTypes, as: H
  alias Exa.Std.Histo
  alias Exa.Std.Histo1D
  alias Exa.Std.Histo2D
  alias Exa.Std.Histo3D
  alias Exa.Std.Tidal
  alias Exa.Std.Mol

  alias Exa.Graf.Adj
  alias Exa.Graf.Dig
  alias Exa.Graf.Traverse
  alias Exa.Graf.Traverse.Visitor
  alias Exa.Graf.DotReader
  alias Exa.Graf.DotWriter, as: DOT

  alias Exa.Graf.DotTypes, as: D

  # ---------
  # constants
  # ---------

  # dispatch map from tag to implementation module
  @disp %{:adj => Adj, :dig => Dig}

  # ----------------
  # public interface
  # ----------------

  @behaviour Exa.Graf.API

  @impl true
  def new(tag, name) do
    # remove punctuation, convert space to '_', truncate for file system
    gname = Exa.String.sanitize!(name, 200)
    dispatch(@disp, tag, :new, [gname])
  end

  @impl true
  def delete(g) when is_graph(g),
    do: dispatch(@disp, g, :delete)

  @impl true
  def vert?(g, i) when is_graph(g) and is_vert(i),
    do: dispatch(@disp, g, :vert?, [i])

  @impl true
  def edge?(g, e) when is_graph(g) and is_edge(e),
    do: dispatch(@disp, g, :edge?, [e])

  @impl true
  def nvert(g) when is_graph(g),
    do: dispatch(@disp, g, :nvert)

  @impl true
  def nedge(g) when is_graph(g),
    do: dispatch(@disp, g, :nedge)

  @impl true
  def verts(g) when is_graph(g),
    do: dispatch(@disp, g, :verts)

  @impl true
  def edges(g) when is_graph(g),
    do: dispatch(@disp, g, :edges)

  @impl true
  def some_vert(g) when is_graph(g) do
    if nvert(g) == 0, do: :error, else: dispatch(@disp, g, :some_vert)
  end

  @impl true
  def add(g, gelem) when is_graph(g),
    do: dispatch(@disp, g, :add, [gelem])

  @impl true
  def delete(g, gelem) when is_graph(g),
    do: dispatch(@disp, g, :delete, [gelem])

  @impl true
  def transpose(g) when is_graph(g),
    do: dispatch(@disp, g, :transpose)

  @impl true
  def degree(g, i, adjy) when is_graph(g) and is_vert(i) and is_adjacency(adjy),
    do: dispatch(@disp, g, :degree, [i, adjy])

  @impl true
  def neighborhood(g, i, adjy) when is_graph(g) and is_vert(i) and is_adjacency(adjy),
    do: dispatch(@disp, g, :neighborhood, [i, adjy])

  @impl true
  def components(g, conn) when is_graph(g) and is_conn(conn),
    do: dispatch(@disp, g, :components, [conn])

  @impl true
  def reachable(g, i, adjy \\ :out, nhop \\ :infinity)
      when is_graph(g) and is_vert(i) and is_adjacency(adjy) and is_nhop(nhop),
      do: dispatch(@disp, g, :reachable, [i, adjy, nhop])

  @impl true
  def condensation(g) when is_graph(g),
    do: dispatch(@disp, g, :condensation)

  # -----------------------
  # generic implementations 
  #     not dispatched 
  # -----------------------

  @doc """
  Create a new graph and add a list of graph elements.
  """
  @spec build(G.gtype(), G.gname(), G.glist()) :: G.graph()
  def build(tag, gname, glist) when is_gname(gname) and is_list(glist) do
    tag |> new(gname) |> add(glist)
  end

  @doc "Get the tag type of a graph."
  @spec tag(G.graph()) :: G.gname()
  def tag(g) when is_graph(g), do: elem(g, 0)

  @doc "Get the name of a graph."
  @spec name(G.graph()) :: G.gname()
  def name(g) when is_graph(g), do: elem(g, 1)

  @doc """
  Change the name of a graph.

  Note that if the graph has an associated set of attributes,
  then the global graph attributes will need to be rekeyed.

  The name argument will be sanitized.
  See `Exa.String.sanitize!/1`.
  """
  @spec rename(G.graph(), G.gname()) :: G.graph()
  def rename(g, gname) when is_graph(g) and is_gname(gname) do
    # remove punctuation, convert space to '_', truncate for file system
    new_name = Exa.String.sanitize!(gname, 200)
    :erlang.setelement(1, g, new_name)
  end

  @doc "Convert a graph to a different format."
  @spec convert(G.graph(), G.gtype()) :: G.graph()

  def convert(g, tag) when tag == elem(g, 0), do: g

  def convert(g, tag) when is_graph(g) and is_gtype(tag) do
    tag |> new(name(g)) |> add(edges(g)) |> add(isolated(g))
  end

  @doc """
  Test if the graph is weakly or strongly connected.

  Returns true if there is exactly one connected component.

  The empty graph is not connected (zero components).

  The graph comprising one isolated vertex is connected,
  because there is exactly one component.
  The isolated vertex may or may not have a self loop.
  """
  @spec connected?(G.graph(), G.connectivity()) :: bool()
  def connected?(g, conn) when is_graph(g) and is_conn(conn) do
    case nvert(g) do
      0 -> false
      1 -> true
      _ -> ncomp(g, conn) == 1
    end
  end

  @doc "Get the number of weakly or strongly connected components."
  @spec ncomp(G.graph(), G.connectivity()) :: E.count()
  def ncomp(g, conn) when is_graph(g), do: g |> components(conn) |> map_size()

  @doc """
  Get the isolated vertices that have no incident edges and no self-edge.
  """
  @spec isolated(G.graph()) :: G.verts()
  def isolated(g) when is_graph(g) do
    g |> verts() |> Enum.filter(fn i -> degree(g, i, :in_out) == {0, 0} end)
  end

  @doc """
  Get the source vertices that have no incoming edges,
  but may have a self-loop. 
  """
  @spec sources(G.graph()) :: G.verts()
  def sources(g) when is_graph(g) do
    Enum.filter(verts(g), fn i -> classify(g, i) in [:source, :self_source] end)
  end

  @doc """
  Get the sink vertices that have no outgoing edges,
  but may have a self-loop. 
  """
  @spec sinks(G.graph()) :: G.verts()
  def sinks(g) when is_graph(g) do
    Enum.filter(verts(g), fn i -> classify(g, i) in [:sink, :self_sink] end)
  end

  @doc """
  Get the vertices as a contiguous range, starting at 1,
  plus a sorted list of additional values after the end of the range.
  """
  @spec verts_rangelist(G.graph()) :: Tidal.range_list()
  def verts_rangelist(g) when is_graph(g) do
    g |> verts() |> Tidal.from_list() |> Tidal.to_range_list()
  end

  @doc "Get the minimum and maximum vertex ids."
  @spec verts_minmax(G.graph()) :: :empty | {min :: G.vert(), max :: G.vert()}
  def verts_minmax(g) when is_graph(g) do
    case verts(g) do
      [] ->
        :empty

      [v | vs] ->
        Enum.reduce(vs, {v, v}, fn i, {vmin, vmax} ->
          {min(vmin, i), max(vmax, i)}
        end)
    end
  end

  @doc "Get the count of self-edges."
  @spec nself(G.graph()) :: E.count()
  def nself(g) do
    Enum.reduce(verts(g), 0, fn i, n -> if edge?(g, {i, i}), do: n + 1, else: n end)
  end

  @doc "Test if there are any self loops."
  @spec self?(G.graph()) :: bool()
  def self?(g), do: Enum.any?(verts(g), fn i -> edge?(g, {i, i}) end)

  @doc """
  Classify a vertex. 

  Values are:
  - `:isolated` no incident edges
  - `:source` only outgoing edges
  - `:sink` only incoming edges
  - `:linear` exactly 1 incoming and 1 outgoing edge, but not a self-loop
  - `:self_isolated` isolated with only a self-loop
  - `:self_source` only outgoing edges and a self-loop
  - `:self_sink` only incoming edges and a self-loop
  - `:self_linear` exactly 1 incoming and 1 outgoing edge, and a self-loop
  - `:complex` both incoming and outgoing edges, 
     with at least 2 incident edges in one direction,
     in addition to any contribution from a self-loop

  Returns an error if the vertex does not exist.
  """
  @spec classify(G.graph(), G.vert()) :: G.vert_class() | {:error, any()}
  def classify(g, i) do
    case degree(g, i, :in_self_out) do
      {:error, _} = err -> err
      {0, 0, 0} -> :isolated
      {_, 0, 0} -> :sink
      {0, 0, _} -> :source
      {1, 0, 1} -> :linear
      {0, 1, 0} -> :self_isolated
      {_, 1, 0} -> :self_sink
      {0, 1, _} -> :self_source
      {1, 1, 1} -> :self_linear
      _ -> :complex
    end
  end

  @doc """
  Test if a vertex has any edges connected to other vertices.

  Equivalent to the classification being `:isolated` or `:self-isolated`.
  """
  @spec isolated?(G.graph(), G.vert()) :: bool()
  def isolated?(g, i), do: classify(g, i) in [:isolated, :self_isolated]

  @doc """
  Test if a graph is a connected tree.

  If the connectivity is `:weak` then the graph is tested
  for being a weakly-connected tree, as if all edges were undirected.
  Returns `true` if it is a single weakly-connected 
  component with `|E| = |V| - 1`.

  If the connectivity is `:strong` then the graph is tested 
  for being a directed rooted tree:
  - weakly-connected tree
  - strongly connected tree
  - a single _root_ source vertex can reach the whole tree 

  Otherwise return `false`.

  Note that in an undirected weak tree, 
  any vertex can be chosen as the root.
  """
  @spec tree?(G.graph(), G.connectivity()) :: bool()
  def tree?(g, :weak), do: nedge(g) == nvert(g) - 1 and connected?(g, :weak)
  def tree?(g, :strong), do: tree?(g, :weak) and is_vert(do_root(g))

  @doc """
  Find a single source vertex that can reach 
  the whole graph along outgoing directed edges.

  The root must have zero in-degree, ignoring any self-loop.
  A vertex with a self-loop can be a directed root,

  For a unique root to exist, the graph must be a 
  single weakly-connected component,
  but not a single strongly-connected component.

  In a single weakly-connected component, 
  any vertex can be chosen as a weakly reachable root.

  In a single strongly-connected component,
  there are no pure sources, 
  as every vertex has both incoming and outgoing edges,
  and any vertex can be chosen as a strongly reachable root.

  For a unique root to exist, the graph may be:
  - directed rooted tree
  - Directed Acyclic Graph (DAG)
  - cyclic graph with more than one 
    strongly connected component.
  """
  @spec root(G.graph()) :: nil | G.vert()
  def root(g), do: if(connected?(g, :weak), do: do_root(g), else: nil)

  defp do_root(g) do
    g
    |> reachable(some_vert(g), :in)
    |> Enum.filter(fn i -> g |> degree(i, :in_self_out) |> elem(0) == 0 end)
    |> is_root(g)
  end

  defp is_root([root], g) do
    if g |> reachable(root, :out) |> MapSet.size() == nvert(g), do: root, else: nil
  end

  defp is_root(_, _), do: nil

  @doc """
  Test if the graph contains a cycle.

  A `:weak` cycle means a cycle of any edges, 
  ignoring direction, as if the graph was undirected.

  A `:strong` cycle means the cycle 
  is aligned with the directions of the edges.

  A self-loop is always a cycle.

  An empty graph is not cyclic.

  Use depth-first search to build spanning trees and test for cycles.
  A cycle is present if:
  - `:weak` an edge to another vertex in the same tree (connected component)
  - `:strong` a reverse edge back to an ancestor in the same tree
  """
  @spec cyclic?(G.graph(), G.connectivity()) :: bool()
  def cyclic?(g, conn) when is_graph(g) and is_conn(conn), do: Traverse.cycle?(g, conn)

  @doc """
  Get a map of frontiers at different hops (radius) from a vertex.
  A frontier at distance _nhop_ is the set difference 
  of the reachable sets at distances `nhop` and `nhop-1`.

  The 0th frontier is just the vertex itself.

  Subsequent frontiers are the vertices included in the next hop
  which have not already been reached.   
  Self-loops do not affect frontiers.

  Traversal stops at the maximum nhop,
  or when the frontier is empty.
  The result does not contain any empty frontier values,
  so it does not necessarily contain all requested nhop values as keys.

  Frontiers are built from recursive exploration of the graph,
  not just reachability traversals with a single adjacency direction.
  For example, the `:in_out` adjacency frontiers 
  will explore the weakly connected frontiers in the graph.

  The union of all frontier values is the total connected set
  for that nhop distance.
  If the graph is a single connected component (weak/in_out, strong/out)
  then the union of an infinite frontier search 
  will include all vertices in the graph.

  Frontiers are disjoint, 
  there is no vertex that appears in more than one frontier.

  Return error if the vertex does not exist.
  """
  @spec frontiers(G.graph(), G.vert(), G.adjacency(), G.nhop()) :: G.frontiers() | {:error, any}
  def frontiers(g, i, adjy \\ :out, nhop \\ :infinity)
      when is_graph(g) and is_vert(i) and is_adjacency(adjy) and is_nhop(nhop) do
    if not vert?(g, i) do
      {:error, "Vertex #{i} does not exist"}
    else
      reach = MapSet.new([i])
      do_frontier(reach, g, adjy, 0, nhop, %{0 => reach})
    end
  end

  # count up to nhop target (never grows equal to :infinity)
  @spec do_frontier(MapSet.t(), G.graph(), G.adjacency(), E.count(), G.nhop(), G.frontiers()) ::
          G.frontiers()

  defp do_frontier(_reach, _g, _adjy, nhop, nhop, fronts), do: fronts

  defp do_frontier(reach, g, adjy, n, nhop, fronts) when is_map_key(fronts, n) do
    front =
      fronts
      |> Map.fetch!(n)
      |> Enum.reduce(MapSet.new(), fn j, fs ->
        g |> reachable(j, adjy, 1) |> MapSet.union(fs)
      end)
      |> MapSet.difference(reach)

    if MapSet.size(front) == 0 do
      fronts
    else
      n1 = n + 1
      new_fronts = Map.put(fronts, n1, front)
      reach |> MapSet.union(front) |> do_frontier(g, adjy, n1, nhop, new_fronts)
    end
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

  @doc """
  Relabel a graph by applying a vertex mapper.

  The result is a new graph, the input graph is not changed.

  If the mapper is a permutation on the same set of vertex ids, 
  or any another 1-1 bijection, then the topology is not changed.

  If the mapper is non-deterministic (randomized), 
  or has at least one many-to-one merging of vertices,
  then the number of vertices, number of edges,
  and topology will change.

  Use `rename/2` to change the name of the output graph.
  """
  @spec relabel(G.graph(), (G.vert() -> G.vert())) :: G.graph()
  def relabel(g, vfun) when is_graph(g) and is_function(vfun, 1) do
    new(elem(g, 0), elem(g, 1))
    |> add(g |> isolated() |> Enum.map(vfun))
    |> add(g |> edges() |> Enum.map(fn {i, j} -> {vfun.(i), vfun.(j)} end))
  end

  @doc """
  Append the second graph to the first graph.

  There are two ways to update the graph:
  - `:merge` vertices and edges maintain the same labels;
     any repeated vertices and edges are ignored

  - `:disjoint` the second graph is relabelled 
     to be independent of the first graph 

  If the graph arguments already have disjoint sets of vertices,
  then the result will be the same for `:merge` and `:disjoint` options.

  The content of the second graph is merged into the first graph.
  The second graph is not modified.

  Merging a graph with itself leaves it unchanged.

  Joining a graph with itself creates two copies. 

  Note that Adj and Dig graphs behave differently under mutation.

  Adj is a functionl data structure, 
  so every update creates a new object.
  Merging Adj graphs is free from side-effects
  and a new graph will be created.

  Dig uses a persistent stateful database 
  managed in a separate process. 
  So updates modify the graph in place.
  Merging Dig graphs _does_ modify the first argument.
  """
  @spec join(G.graph(), G.graph(), :merge | :disjoint) :: G.graph()

  def join(g1, g2, :merge) when is_graph(g1) and is_graph(g2) do
    g1 |> add(isolated(g2)) |> add(edges(g2))
  end

  def join(g1, g2, :disjoint) when is_graph(g1) and is_graph(g2) do
    {min1, max1} = verts_minmax(g1)
    {min2, max2} = verts_minmax(g2)

    if max1 < min2 or max2 < min1 do
      join(g1, g2, :merge)
    else
      # shift the second vert range to be above the first
      # note relabel creates a new graph, so g2 remains unchanged
      delta = max1 - min2 + 1
      join(g1, relabel(g2, fn i -> i + delta end), :merge)
    end
  end

  @doc """
  Contract an edge.

  Merge the two nodes at the ends of an edge.
  The remaining node will have the minimum of the two vertex ids.
  The min node will have edges added 
  for all in/out neighbors of the max node.
  Any duplicated edges will be ignored.
  The max node and all its edges will be deleted.

  If the target edge is a self-edge,
  then it will be deleted,
  but no other changes are made.

  If the max node had a self-edge, 
  a self-edge will be added to the min node.

  If the target edge does not exist, there is no effect,
  and the result will be the same as the input.
  Otherwise, the input graph will be modified.
  """
  @spec contract_edge(G.graph(), G.edge()) :: G.graph()
  def contract_edge(g, {src, dst} = e) do
    cond do
      not edge?(g, e) ->
        g

      src == dst ->
        delete(g, e)

      true ->
        {i, j} = if src < dst, do: e, else: {dst, src}
        {ins, self, outs} = neighborhood(g, j, :in_self_out)

        # transfer any self-edge 
        g = if is_nil(self), do: g, else: g |> add({i, i}) |> delete({j, j})

        # transfer inward edges 
        # transfer outward edges
        # deleting the node deletes all its edges
        g
        |> add(Enum.map(ins, &{&1, i}))
        |> add(Enum.map(outs, &{i, &1}))
        |> delete(j)
    end
  end

  @doc """
  Contract a collection of distinct vertices onto a single vertex.
  The vertices do not have to be linked by edges.

  The remaining vertex will have the minimum of the vertex ids.

  The minimum node will have edges added 
  for all external neighbors of the node group,
  plus the transfer of any self-edge.

  The remainder of the node group and all its edges will be deleted.

  Any duplicated edges will be ignored.
  Any repeated vertices will have no effect.

  If only 0 or 1 target vertices exist, there is no effect,
  and the result will be the same as the input.
  Otherwise, the input graph will be modified.
  """
  @spec contract_nodes(G.graph(), G.verts() | G.vset()) :: G.graph()
  def contract_nodes(g, verts) do
    # filter non-existent/duplicate vertices and get min vertex value
    {imin, vset} =
      Enum.reduce(verts, {nil, @empty_set}, fn i, {imin, vset} = acc ->
        if vert?(g, i), do: {minil(imin, i), MapSet.put(vset, i)}, else: acc
      end)

    if MapSet.size(vset) < 2 do
      g
    else
      vset
      |> MapSet.delete(imin)
      |> Enum.reduce(g, fn j, g ->
        {jins, jself, jout} = neighborhood(g, j, :in_self_out)

        g
        |> add_self(imin, jself)
        |> add(jins |> MapSet.difference(vset) |> Enum.map(&{&1, imin}))
        |> add(jout |> MapSet.difference(vset) |> Enum.map(&{imin, &1}))
        |> delete(j)
      end)
    end
  end

  defp add_self(g, _i, nil), do: g
  defp add_self(g, i, _j), do: add(g, {i, i})

  defp minil(nil, k), do: k
  defp minil(i, k), do: min(i, k)

  @doc """
  Contract a linear node.

  A linear node has exactly one incoming edge, 
  one outgoing edge and no self-loop.

  The node is removed, and the two edges are replaced by a
  single edge from the incoming neighbor to the outgoing neighbor.

  There are two additional constraints on the new edge:
  - not a duplicate of an existing edge
  - not a self-loop

  If the neighbors are the same node, 
  no self-loop is created.

  So, if the input graph is simple (no self-loops)
  then the output graph is also simple.

  Contracting linear nodes does not change the 
  topological structure of the graph.

  The topologies of two graphs can be compared 
  by contracting all linear nodes and testing for isomorphism.
  If the two contractions are isomorphic, 
  then the original graphs are _homeomorphic_ (topologically equivalent).
  """
  @spec contract_linear(G.graph(), G.vert()) :: G.graph() | {:error, any()}
  def contract_linear(g, i) when is_graph(g) and is_vert(i) do
    case neighborhood(g, i, :in_self_out) do
      {:error, _} = err ->
        err

      {_ins, ^i, _outs} ->
        {:error, "Self loop"}

      {ins, nil, outs} ->
        if MapSet.size(ins) != 1 or MapSet.size(outs) != 1 do
          {:error, "Not linear"}
        else
          [j] = MapSet.to_list(ins)
          [k] = MapSet.to_list(outs)

          cond do
            j == k -> {:error, "Creates self-loop"}
            edge?(g, {j, k}) -> {:error, "Edge exists"}
            true -> g |> delete(i) |> add({j, k})
          end
        end
    end
  end

  @doc """
  Contract all linear nodes.

  Contracting linear nodes preserves topological structure,
  so it is a _homeomorphism_ (topological isomorphism).

  See `contract_linear/2`.
  """
  @spec contract_linears(G.graph()) :: G.graph()
  def contract_linears(g) when is_graph(g) do
    Enum.reduce(verts(g), g, fn i, g ->
      case contract_linear(g, i) do
        {:error, _} -> g
        new_g -> new_g
      end
    end)
  end

  @doc """
  Compare two graphs for _homeomorphism_ (topological equivalence). 

  Two graphs are _homeomorphic_ if their 
  linear contractions are isomorphic.

  Linear node contraction just reduces the count of
  nodes with 3-degree (`in_self_out`) value `{1,0,1}`.
  So two graphs are _not_ homeomorphic if their 
  3D degree histograms differ outside the `{1,0,1}` bin.

  Currently, the isomorphism test never returns `true`,
  so the most positive result is `:undecided`.
  """
  @spec homeomorphic?(G.graph(), G.graph()) :: false | :undecided
  def homeomorphic?(g1, g2) when is_graph(g1) and is_graph(g2) do
    if g1 |> degree_histo3d() |> Map.delete({1, 0, 1}) !=
         g2 |> degree_histo3d() |> Map.delete({1, 0, 1}) do
      false
    else
      isomorphic?(contract_linears(g1), contract_linears(g2))
    end
  end

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
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      Histo1D.inc(h, degree(g, i, adjy))
    end)
  end

  def degree_histo1d(g, :in_out) do
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      {indeg, outdeg} = degree(g, i, :in_out)
      Histo1D.inc(h, indeg + outdeg)
    end)
  end

  def degree_histo1d(g, :in_self_out) do
    Enum.reduce(verts(g), Histo1D.new(), fn i, h ->
      {indeg, _, outdeg} = degree(g, i, :in_self_out)
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
    Enum.reduce(verts(g), Histo2D.new(), fn i, h ->
      Histo2D.inc(h, degree(g, i, :in_out))
    end)
  end

  def degree_histo2d(g, :in_self_out) do
    Enum.reduce(verts(g), Histo2D.new(), fn i, h ->
      {indeg, _, outdeg} = degree(g, i, :in_self_out)
      Histo2D.inc(h, {indeg, outdeg})
    end)
  end

  @doc "Create a 3D histogram of the (in, self, out) vertex degrees."
  @spec degree_histo3d(G.graph()) :: H.histo3d()
  def degree_histo3d(g) do
    Enum.reduce(verts(g), Histo3D.new(), fn i, h ->
      Histo3D.inc(h, degree(g, i, :in_self_out))
    end)
  end

  @doc """
  Test two graphs for exact equality,
  including all vertex and edge identifiers.

  This may be very slow, consider spawning as a task:
  - digraph (dig) is stored in ETS (separate process)
    so `Task` asynch/await may be appropriate
  - adj is in-process, so a spawned task will
    incur an overhead for copying all graph data 
  """
  @spec equal?(G.graph(), G.graph()) :: bool()
  def equal?(g1, g2) when is_graph(g1) and is_graph(g2) do
    case isomorphic?(g1, g2) do
      false ->
        false

      :undecided ->
        g1 |> verts() |> MapSet.new() == g2 |> verts() |> MapSet.new() and
          g1 |> edges() |> MapSet.new() == g2 |> edges() |> MapSet.new()
    end
  end

  @doc """
  Test two graphs for isomorphism,
  which means a structural equivalence ignoring all vertex identitiers.
  For isomorphic graphs, there is a 1-1 bijective relabelling of all vertices
  that will make the graphs exactly equal.

  The result is either `false` or `:undecided`.
  A full isomorphism test is not attempted.

  The test compares:
  - number of vertices
  - number of edges
  - hashes of the graphs

  Hashes are currently based on the  
  3D histogram of vertex in-self-out degrees.

  This is a relatively quick check to exclude many non-isomorphic pairs.
  It also does not do a full equality check.
  """
  @spec isomorphic?(G.graph(), G.graph()) :: false | :undecided
  def isomorphic?(g1, g2) when is_graph(g1) and is_graph(g2) do
    if nvert(g1) == nvert(g2) and
         nedge(g1) == nedge(g2) and
         hash(g1, 0) == hash(g2, 0) and
         hash(g1, 1) == hash(g2, 1) do
      # next steps not currently implemented:
      # - no isomorphism permutation check
      # - no test for equality here
      :undecided
    else
      false
    end
  end

  @doc """
  Create a hash of the graph.

  The hash should reasonably discriminate graphs
  by their topology, with a simple and relatively fast algorithm.
  The hash ignores vertex and edge identifiers.

  The hash can be used to reject a graph isomorphism test.
  Graphs with different hashes cannot be isomorphic.
  Graphs with the same hash may be isomorphic, or not,
  they are _undecided._

  The basic approach is to:
  - create a histogram from an encoding of 
    local vertex neighborhood topologies
  - convert the histogram term to binary
  - hash the binary using SHA-256
  - convert the hash to a 256-bit unsigned integer

  There are two levels of encoding the local topology of a vertex,
  based on the number of hops out from the vertex:
  - 0 hop: 3-tuple of *in_self_out* degrees for the vertex itself
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
    g |> degree_histo3d() |> hash_term()
  end

  def hash(g, 1) do
    verts = verts(g)

    # build two indexes of neighborhood described as:
    #   {in_neighbors, out_neighbors} exluding self loops
    #   {in_degree, self_degree, out_degree}
    {neigh_index, deg_index} =
      Enum.reduce(verts, {%{}, %{}}, fn i, {nindex, dindex} ->
        {ins, self, outs} = neighborhood(g, i, :in_self_out)
        self_deg = if is_nil(self), do: 0, else: 1

        {
          Map.put(nindex, i, {ins, outs}),
          Map.put(dindex, i, {MapSet.size(ins), self_deg, MapSet.size(outs)})
        }
      end)

    # build a histogram of vertex hashes
    # calculated from on neighborhood tuple
    # encoded using 3-degrees (in_self_out 3-tuples)
    vhash_histo =
      Enum.reduce(verts, Histo.new(), fn i, h ->
        {ins, outs} = Map.fetch!(neigh_index, i)
        in_histo3d = do_histo3d(ins, deg_index)
        self_degree3 = Map.fetch!(deg_index, i)
        out_histo3d = do_histo3d(outs, deg_index)
        vdata = {in_histo3d, self_degree3, out_histo3d}
        vhash = hash_term(vdata)
        Histo.inc(h, vhash)
      end)

    # finally, hash the vertex hash histogram
    hash_term(vhash_histo)
  end

  # index of vertex id to 3-degree tuple (in_self_out)
  @typep degree_index3() :: %{G.vert() => {G.degree(), 0 | 1, G.degree()}}

  # build a 3D histogram from indexed 3-degrees (in_self_out)
  @spec do_histo3d(G.verts(), degree_index3()) :: H.histo3d()
  defp do_histo3d(verts, deg_index) do
    Enum.reduce(verts, Histo3D.new(), fn i, h ->
      Histo3D.inc(h, Map.fetch!(deg_index, i))
    end)
  end

  # hash any term to produce a 256-bit unsigned integer
  @spec hash_term(any()) :: G.hash()
  defp hash_term(term) do
    <<i::256>> = :crypto.hash(:sha256, :erlang.term_to_binary(term, [:local]))
    i
  end

  # --------
  # file I/O
  # --------

  @doc """
  Write a graph to file in GraphViz DOT format.

  The graph `gname` is used as the title of the DOT graph object,
  as a key for global properties in the graph attribute map,
  and as the basename for the output file.

  Return the DOT text as `IO.chardata` and the full output filename.

  Use `Exa.Dot.Render.render_dot/3` 
  to convert the DOT file to PNG or SVG.
  """
  @spec to_dot_file(G.graph(), E.filename(), D.graph_attrs()) ::
          {E.filename(), IO.chardata()}
  def to_dot_file(g, dotdir, gattrs \\ %{})
      when is_graph(g) and is_filename(dotdir) do
    Exa.File.ensure_dir!(dotdir)
    gname = name(g)
    filename = Exa.File.join(dotdir, gname, [@filetype_dot])

    dot =
      DOT.new_dot(gname)
      |> DOT.globals(gname, gattrs)
      |> DOT.nodes(verts(g), gattrs)
      |> DOT.edges(edges(g), gattrs)
      |> DOT.end_dot()
      |> DOT.to_file(filename)

    {filename, dot}
  end

  @doc """
  Read a graph from GraphViz DOT file.

  The graph name is read from the internal DOT digraph name,
  not taken from the file basename.
  """
  @spec from_dot_file(G.gtype(), E.filename()) :: {G.graph(), D.graph_attrs()}
  def from_dot_file(tag \\ :adj, filename) when is_gtype(tag) and is_filename(filename) do
    DotReader.from_dot_file(tag, filename)
  end

  @doc "Read an graph from an ADJ file in Elixir literal format."
  @spec from_adj_file(G.gtype(), E.filename()) :: G.graph() | {:error, any()}
  def from_adj_file(tag \\ :adj, filename) when is_gtype(tag) and is_filename(filename) do
    case Adj.from_adj_file(filename) do
      {:error, _} = err -> err
      adj -> convert(adj, tag)
    end
  end

  @doc """
  Write a graph to an ADJ file in Elixir literal format.

  Return the full output filename.
  """
  @spec to_adj_file(G.graph(), E.filename()) :: E.filename() | {:error, any()}
  def to_adj_file(g, outdir) when is_graph(g) and is_filename(outdir) do
    g |> convert(:adj) |> Adj.to_adj_file(outdir)
  end

  # ----------------
  # graph traversals
  # ----------------

  @doc """
  Build a spanning forest for the graph.

  A spanning forest is a sequence of rooted trees:
  - weak: arbitrary edge directions
  - strong: edges directed from the root

  The forest includes all vertices, but only a subset of the edges.

  For weak connectivity, edges are traversed in any direction
  and the tree does not have consistent edge directions.
  Any edge that would connect within the same tree is excluded (cycle).
  There is one tree for each weakly connected component. 
  There are no cross edges between trees.

  For strong connectivity, edges are only traversed in the 
  forward direction, and trees are directed from root to leaves. 

  These classes of edges are excluded from the strong forest:
  - reverse: to ancestors within the same tree (cycles)
  - forward: to non-ancestors on other branches within the same tree (dag)
  - reverse cross: from a later tree to an earlier tree

  By construction, there are no _forward cross_ edges
  from earlier trees to later trees.

  Self-loops do not affect the forest,
  because the source of the edge is already in the tree.

  The forest is represented as a map of vertices 
  to a list of their outgoing directed tree edges (MoL).
  Leaf vertices with no outgoing tree edges 
  do not have an entry in the forest (MoL empty default semantics).
  There is a special key `:forest` that 
  contains a list of the roots of trees.

  The spanning forest is created by a depth-first traversal,
  so it is often called a _depth-first forest_ (DFF).

  The initial root vertex for the depth-first search may be specified
  as an argument, otherwise the root is picked arbitrarily from the graph.
  """
  @spec spanning_forest(G.graph(), G.connectivity(), nil | G.vert()) :: G.forest()
  def spanning_forest(g, conn, root \\ nil)
      when is_graph(g) and (is_nil(root) or is_vert(root)) and is_conn(conn) do
    cb = %Visitor{
      # a DFF is an Map of Lists; empty stack List of Lists for children
      init_state: fn _g -> {Mol.new(), [[]]} end,
      pre_tree: fn {dff, lol}, _g, root ->
        # add new root to forest
        {Mol.append(dff, :forest, root), lol}
      end,
      pre_node: fn {dff, lol}, _g, _i ->
        # push an empty list of children onto the stack
        {dff, [[] | lol]}
      end,
      post_node: fn {dff, [js, is | lol]}, _g, i ->
        # pop the stack, build the tree adjacency, push i into the parent child list
        {Mol.set(dff, i, Enum.reverse(js)), [[i | is] | lol]}
      end,
      final_result: fn {dff, [_]} -> dff end
    }

    Traverse.graph(g, :dfs, conn, cb, root)
  end

  # -----------------
  # forest traversals
  # -----------------

  @doc "Print out an indented view of the traversal of a Depth First Forest."
  @spec dump_forest(G.forest()) :: nil
  def dump_forest(dff) when is_forest(dff) do
    Traverse.forest(
      dff,
      %Visitor{
        init_state: fn _ -> ["  "] end,
        pre_node: fn indent, _g, i ->
          IO.puts([indent, "#{i} pre"])
          ["  " | indent]
        end,
        visit_node: fn indent, _g, i ->
          IO.puts([indent, "#{i} leaf"])
          indent
        end,
        post_node: fn indent, _g, i ->
          indent = tl(indent)
          IO.puts([indent, "#{i} post"])
          indent
        end
      }
    )
  end

  @doc "Get a pre-order traversal sequence of vertices."
  @spec preorder(G.forest()) :: G.verts()
  def preorder(dff) when is_forest(dff) do
    Traverse.forest(
      dff,
      %Visitor{
        init_state: fn _ -> {[], []} end,
        pre_node: fn {path, ord}, _g, i -> {[i | path], [i | ord]} end,
        visit_node: fn {path, ord}, _g, i -> {path, [i | ord]} end,
        post_node: fn {[_ | path], ord}, _g, _i -> {path, ord} end,
        final_result: fn {[], ord} -> Enum.reverse(ord) end
      }
    )
  end

  @doc "Get a post-order traversal sequence of vertices."
  @spec postorder(G.forest()) :: G.verts()
  def postorder(dff) when is_forest(dff) do
    Traverse.forest(
      dff,
      %Visitor{
        init_state: fn _ -> {[], []} end,
        pre_node: fn {path, ord}, _g, i -> {[i | path], ord} end,
        visit_node: fn {path, ord}, _g, i -> {path, [i | ord]} end,
        post_node: fn {[_ | path], ord}, _g, i -> {path, [i | ord]} end,
        final_result: fn {[], ord} -> Enum.reverse(ord) end
      }
    )
  end
end
