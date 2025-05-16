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

  alias Exa.Types, as: E

  import Exa.Dispatch, only: [dispatch: 4, dispatch: 3]

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Std.Tidal

  import Exa.Std.Mos

  alias Exa.Graf.Adj
  alias Exa.Graf.Dig
  alias Exa.Graf.Traverse

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

  @impl true

  # let the concrete implementations optimize their internal equalities
  # otherwise implement a default algorithm comparing graph elements

  def equal?({:adj, _, _} = g1, {:adj, _, _} = g2),
    do: dispatch(@disp, g1, :equal?, [g2])

  def equal?({:dig, _, _} = g1, {:dig, _, _} = g2),
    do: dispatch(@disp, g1, :equal?, [g2])

  def equal?(g1, g2) when is_graph(g1) and is_graph(g2) do
    nvert(g1) == nvert(g2) and
      nedge(g1) == nedge(g2) and
      do_equal(g1, g2)
  end

  # keep the vert/edge comparison separate so it can be 
  # invoked without repeating nvert/nedge equality check
  defp do_equal(g1, g2) do
    MapSet.new(verts(g1)) == MapSet.new(verts(g2)) and
      MapSet.new(edges(g1)) == MapSet.new(edges(g2))
  end

  # -----------------------
  # generic implementations 
  #     not dispatched 
  # -----------------------

  @doc """
  Assert that a vertex is a member of the graph,
  otherwise throw an `ArgumentError`.
  """
  def assert_vert!(g, i) do
    if not vert?(g, i) do
      raise ArgumentError, message: "Vertex #{i} does not exist"
    end
  end

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
    :erlang.setelement(2, g, new_name)
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
  Invert a disjoint partition of a graph (e.g. component or frontier map).
  The partition may be total or partial (some missing vertices).

  The result is a pair of indexes for vertices and edges.
  The vertex index maps vertices to partition id.
  The edge index maps edges to their lifted partition edge.

  The partition indexes always contain keys 
  for all vertices and edges in the graph.

  If the partition is total, then 
  all the vertex and edge entries 
  will have valid partition values.

  If the partition is partial, then 
  the vertex index will contain `nil` values,
  and the edge index will contain values with 1 or 2 `nil` endpoints.
  """
  @spec partition_index(G.graph(), G.partition()) :: G.partition_index()
  def partition_index(g, parts) when is_graph(g) and is_mos(parts) do
    vidx =
      Enum.reduce(parts, %{}, fn {ipart, iset}, vidx ->
        Enum.reduce(iset, vidx, fn i, vidx -> Map.put(vidx, i, ipart) end)
      end)

    vidx =
      if nvert(g) == map_size(vidx) do
        # total 
        vidx
      else
        # partial - add missing vertices
        Enum.reduce(verts(g), vidx, fn
          i, vidx when is_map_key(vidx, i) -> vidx
          i, vidx -> Map.put(vidx, i, nil)
        end)
      end

    eidx =
      Enum.reduce(edges(g), %{}, fn {i, j} = e, eidx ->
        Map.put(eidx, e, {vidx[i], vidx[j]})
      end)

    {vidx, eidx}
  end

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
  A vertex with a self-loop can be a directed root.

  For a unique root to exist, the graph must be a 
  single weakly-connected component,
  and the required connectivity must be strong.

  In a single weakly-connected component, 
  any vertex can be chosen as a weakly reachable root.

  In a single strongly-connected component,
  there are no pure sources, 
  as every vertex has both incoming and outgoing edges,
  and any vertex can reach all others.

  For a unique root to exist for strong connectivity, 
  the graph must be a Directed Acyclic Graph (DAG),
  which includes directed rooted tree as a special case.
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

  Also see `Exa.Graf.Morf.frontier_histo1d/1`.
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
  Relabel a graph by applying a vertex mapper.
  The mapper can be a map data structure, or a mapping function. 

  The result is a new graph, the input graph is not changed.

  If the mapper is a permutation on the same set of vertex ids, 
  or any another 1-1 bijection, then the topology is not changed.

  If the mapper has one or more many-to-one merging of vertices,
  then the number of vertices, number of edges,
  and topology will change.

  Use `rename/2` to change the name of the output graph.
  """
  @spec relabel(G.graph(), E.mapping(G.vert(), G.vert())) :: G.graph()

  def relabel(g, vmap) when is_graph(g) and is_map(vmap) do
    new(elem(g, 0), elem(g, 1))
    |> add(g |> isolated() |> Enum.map(&Map.fetch!(vmap, &1)))
    |> add(g |> edges() |> Enum.map(fn {i, j} -> {vmap[i], vmap[j]} end))
  end

  def relabel(g, vfun) when is_graph(g) and is_function(vfun, 1) do
    new(elem(g, 0), elem(g, 1))
    |> add(g |> isolated() |> Enum.map(vfun))
    |> add(g |> edges() |> Enum.map(fn {i, j} -> {vfun.(i), vfun.(j)} end))
  end

  @doc """
  Relabel a graph by applying a 
  simple permutation to the node labels.

  The permutation is a 1-step rotation of the existing labels,
  where each node is relabelled with the next higher label,
  except the highest label is replaced by the lowest.

  If the existing labels form a simple integer range `1..n`
  then the permutation function for label _i_ will be: 

  `i -> 1 + rem(i, nvert)`

  If the graph is empty, the same empty graph is returned.

  Applying the rotation `nvert` times will give the original graph.
  """
  @spec rotate(G.graph()) :: G.graph()
  def rotate(g) when is_graph(g) do
    case nvert(g) do
      0 ->
        g

      n ->
        verts = g |> verts() |> Enum.sort()

        case {hd(verts), List.last(verts)} do
          {1, ^n} -> relabel(g, fn i -> 1 + rem(i, n) end)
          {lo, hi} -> relabel(g, rot1(verts, %{hi => lo}))
        end
    end
  end

  @spec rot1(G.verts(), G.vmap()) :: G.vmap()
  defp rot1([_hi], vmap), do: vmap
  defp rot1([x | [y | _] = t], vmap), do: rot1(t, %{vmap | x => y})

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

  # choose Map, Ord or Tree
  # @heap Exa.Std.MinHeap.Map

  # @doc """
  # Single source shortest path traversal.

  # Finds a path of shortest length
  # between source and destination vertices.
  # If there is more than one shortest path,
  # an arbitrary instance is returned.

  # Connectivity controls the way edges are traversed:
  # - `:weak` both incoming and outgoing edges
  # - `:strong` (default) only outgoing edges

  # It is an error if the source or destination vertex
  # does not exist in the graph.
  # """
  # @spec sssp(G.graph(), G.vert(), G.vert(), G.connectivity()) :: [G.path()]
  # def sssp(g, src, dst, conn \\ :strong) when is_graph(g) and is_vert(src) and is_vert(dst) do
  #   assert_vert!(g, src)
  #   assert_vert!(g, dst)

  #   Traverse.graph(
  #     g,
  #     :bfs,
  #     conn,
  #     %Visitor{
  #       init_state: fn _ -> {%{}, []} end,
  #       test_node: fn
  #         # TODO - empty case when src=dst at root
  #         # {pmap, []=path}, _g, ^dst -> 
  #         #  {:halt, {Map.put(pmap, dst, par), [dst | path]}}
  #         {pmap, [par | _] = path}, _g, ^dst ->
  #           {:halt, {Map.put(pmap, dst, par), [dst | path]}}

  #         _state, _g, _i ->
  #           :continue
  #       end,
  #       pre_branch: fn {pmap, [par | _] = path}, _g, i ->
  #         {Map.put(pmap, i, par), [i | path]}
  #       end,
  #       visit_leaf: fn {pmap, [par | _] = path}, _g, i ->
  #         {Map.put(pmap, i, par), [i | path]}
  #       end,
  #       post_branch: fn {pmap, path}, _g, _i -> {pmap, tl(path)} end,
  #       final_result: fn {pmap, _path} -> pmap end
  #     },
  #     src
  #   )
  # end

  # @doc """
  # Dijkstra's algorithm for single source all distances traversal.

  # Finds a map of shortest distances
  # between a source vertex and all other reachable vertices.

  # Connectivity controls the way edges are traversed:
  # - `:weak` both incoming and outgoing edges
  # - `:strong` (default) only outgoing edges

  # It is an error if the source vertex
  # does not exist in the graph.
  # """
  # @spec dijkstra(G.graph(), G.vert(), G.vert(), G.connectivity()) :: [G.path()]
  # def dijkstra(g, src, dst, conn \\ :strong) when is_graph(g) and is_vert(src) and is_vert(dst) do
  #   assert_vert!(g, src)
  #   assert_vert!(g, dst)

  #   # fn path, _g, i -> [i | path] end

  #   Traverse.graph(
  #     g,
  #     heap_traversor(),
  #     conn,
  #     %Visitor{
  #       init_state: fn _ -> [] end,
  #       test_node: fn
  #         path, _g, ^dst -> {:halt, [dst | path]}
  #         _path, _g, _i -> :continue
  #       end,
  #       pre_branch: fn path, _g, i -> [i | path] end,
  #       # visit_leaf: fn path, _g, i -> [i | path] end,
  #       post_branch: fn path, _g, _i -> tl(path) end,
  #       final_result: fn path -> Enum.reverse(path) end
  #     },
  #     src
  #   )
  # end

  # defp heap_traversor() do
  #   %Traversor{
  #     :init_vdata => fn g ->
  #       # replace with new(map)
  #       heap =
  #         Enum.reduce(verts(g), @heap.new(), fn i, h ->
  #           MinHeap.add(h, i, :inf)
  #         end)

  #       IO.inspect(heap, label: "init")

  #       {0, heap}
  #     end,
  #     :pop_node => fn {_d, heap} ->
  #       case MinHeap.pop(heap |> IO.inspect(label: "pop")) do
  #         # TODO - end of traversal
  #         :empty -> {:empty, heap}
  #         {{i, d}, heap} -> {i, {d, heap}}
  #       end
  #     end,
  #     :push_nodes => fn {d, heap}, front ->
  #       IO.inspect(front)
  #       # TODO - fake
  #       type = if MapSet.size(front) == 1, do: :leaf, else: :branch
  #       # all edges are given weight 1
  #       d = d + 1
  #       IO.inspect(heap, label: "push #{d}")

  #       heap =
  #         Enum.reduce(front, heap, fn i, h ->
  #           idist = MinHeap.get(h, i) |> IO.inspect(label: "idist")
  #           if not is_nil(idist) and d < idist, do: MinHeap.update(h, i, d), else: h
  #         end)

  #       {type, {d, heap}}
  #     end,
  #     select_root: fn {0, heap}, r when not is_nil(r) ->
  #       # TODO - nil means end of component
  #       {r, {0, MinHeap.update(heap, r, 0) |> IO.inspect(label: "select")}}
  #     end
  #   }
  # end
end
