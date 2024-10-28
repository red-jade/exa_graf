defmodule Exa.Graf.Types do
  @moduledoc "Types and guards for graphs."

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Std.Mol, as: Mol
  alias Exa.Std.Mos, as: Mos

  # graf format ----------

  @typedoc "A vertex id is a positive integer."
  @type vert() :: pos_integer()

  defguard is_vert(i) when is_int_pos(i)

  @typedoc "List of vertices."
  @type verts() :: [vert()]

  @typedoc "A contiguous sequence of vertex ids."
  @type vseq() :: Range.t()

  @typedoc "A set of vertices."
  @type vset() :: MapSet.t(vert())

  # relabelling of vertices (1-1 bijection)
  @type vmap() :: %{G.vert() => G.vert()}

  @typedoc "A directed edge is an ordered pair of vertices."
  @type edge() :: {vert(), vert()}

  defguard is_edge(e)
           when is_tuple_fix(e, 2) and
                  is_vert(elem(e, 0)) and is_vert(elem(e, 1))

  @typedoc "A chain of edges is a tuple of vertices."
  @type chain() :: tuple()

  defguard is_chain(t) when is_tuple(t) and tuple_size(t) > 2

  @typedoc "List of edges."
  @type edges() :: [edge()]

  @typedoc "A single out-adjacency list."
  @type adjlist() :: {vert(), verts()}

  # TODO? - a small space optimization is to make adjmap defaults for m=0,1
  #   m=0  {src, nil or []}
  #   m=1  {src, dst}
  #   m>1  {src, MapSet}
  # good for sparse graphs

  @typedoc """
  Adjacency map of vertex to set of neighbor vertices.

  Isolated vertices have an entry with the empty set.
  """
  @type adjmap() :: Mos.mos(vert(), vert())

  @typedoc "In and out adjacency maps."
  @type adjmaps() :: {inadj :: adjmap(), outadj :: adjmap()}

  @typedoc """
  Graph elements:
  - vertex
  - vertex range
  - edge
  - chain of edges
  - out adjacency list 
  - list of all of the above
  """
  @type gelem() :: vert() | vseq() | edge() | chain() | adjlist() | glist()

  @typedoc "List of graph elements."
  @type glist() :: [gelem()]

  @typedoc "Graphs have a string name."
  @type gname() :: String.t()

  defguard is_gname(gname) when is_string_nonempty(gname)

  @typedoc """
  An adjacency graph data structure.

  Graph elements are stored in two adjacency maps 
  for in and out neighbors.

  The keys for both maps are the set of vertices,
  so the map size is the number of vertices (nverts),
  and will be the same for both maps.

  The total size of all value sets in the maps 
  is the number of edges (nedge), 
  and will be the same for both maps.

  Self-loops are allowed.

  Repeated edges are not supported.
  """
  @type adj() :: {:adj, gname(), adjmaps()}

  defguard is_adj(a) when is_tuple_tag(a, 3, :adj) and is_gname(elem(a, 1))

  @typedoc """
  A named Erlang digraph.

  Graph elements are stored in ETS.

  Repeated edges are not supported.
  """
  @type dig() :: {:dig, gname(), :digraph.graph()}

  defguard is_dig(g) when is_tuple_tag(g, 3, :dig)

  @typedoc "The set of tags for graph tuple types."
  @type gtype() :: :adj | :dig

  defguard is_gtype(t) when t in [:adj, :dig]

  @typedoc """
  A supertype for all graph types.

  A graphs must be a tagged 3-tuple, with name and specific data.
  """
  @type graph() :: adj() | dig()

  defguard is_graph(g) when is_tuple_fix(g, 3) and is_gtype(elem(g, 0))

  # -------------------------------------------
  # counts, neighborhoods, adjacency and degree 
  # -------------------------------------------

  @typedoc """
  Degree is the count of edges for a vertex.

  The scalar degree is a single number for adjacencies:
  - `:in` number of incoming edges
  - `:out` number of outgoing edges

  If the vertex has a self-loop, 
  the loop is included in each of the scalar counts.
  """
  @type degree() :: E.count()

  @typedoc """
  Degrees are counts of edges for a vertex.

  The degree is a pair for adjacency:
  - `:in_out` degrees of incoming and outgoing edges

  If the vertex has a self-loop, 
  the loop is included in both counts.
  """
  @type degree2() :: {n_in :: G.degree(), n_out :: G.degree()}

  @typedoc """
  Degrees are counts of edges for a vertex.

  The degree is a triple for adjacency:
  - `:in_self_out` degrees of incoming edges, self-loop, 
     and outgoing edges

  If the vertex has a self-loop, 
  it is counted separately in the central value,
  not included in the _in_ or _out_ counts.
  """
  @type degree3() :: {n_in :: G.degree(), self :: 0 | 1, n_out :: G.degree()}

  @typedoc """
  A set of neighbors. 

  The scalar is a single set for adjacencies:
  - `:in` incoming neighbors
  - `:out` outgoing neighbors

  If the vertex has a self-loop, 
  the vertex is included in each of the scalar sets.
  """
  @type neigh() :: vset()

  @typedoc """
  The sets of neighbors for a vertex.

  The neighbors are a pair of sets for adjacency:
  - `:in_out` separate in/out neighbors

  If the vertex has a self-loop, 
  the vertex is included in both of the two sets.
  """
  @type neigh2() :: {ins :: neigh(), outs :: neigh()}

  @typedoc """

  The sets of neighbors for a vertex.
  The neighbors are a triple for adjacency:
  - `:in_self_out` in, self and out as separate fields

  If the vertex has a self-loop, 
  it is indicated in the central value,
  not included in the _in_ or _out_ sets.  
  """
  @type neigh3() :: {ins :: neigh(), self :: nil | G.vert(), outs :: neigh()}

  @typedoc """
  Type of adjacency neighborhood:
  - `:in` incoming edges and upstream neighbors
  - `:out` outgoing edges and downstream neighbors
  - `:in_out` combined incident edges and all adjacent neighbors
  - `:in_self_out` all adjacency, but separate self-loop 

  A vertex is always self-adjacent (reachable from itself), 
  even if it does not have a self-loop,

  """
  @type adjacency() :: :in | :out | :in_out | :in_self_out

  defguard is_adjacency(a) when a in [:in, :out, :in_out, :in_self_out]

  @typedoc """
  The number of hops in a path through the graph.

  Each edge counts as one unit.

  A hop of zero includes just the source vertex.

  A value of `:infinity` means explore with traversal 
  to the limits of the graph, 
  under some other termination constraint.

  """
  @type nhop() :: E.count() | :infinity

  defguard is_nhop(n) when is_count(n) or n == :infinity

  @typedoc """
  The frontiers at increasing hops (radius) from a vertex.

  A frontier at distance _nhop_ is the set difference 
  of the reachable sets at distances `nhop` and `nhop-1`.

  The 0th frontier is just the vertex itself.

  Subsequent frontiers are the vertices included in the next hop
  which have not already been reached. 

  The frontier index does not contain any empty frontier values.

  The union of all frontier values is the total reachable set
  for that nhop distance.

  If the graph is a single connected component (weak/in_out, strong/out)
  then the union of an infinite frontier search 
  will include all vertices in the graph.

  Frontiers are disjoint, 
  there is no vertex that appears in more than one frontier.
  """
  @type frontiers() :: %{E.count() => vset()}

  @typedoc """
  Connectivity for components:
  - Weakly connected: each pair of vertices is connected
    by at least one undirected path traversing 
    edges in any direction.
  - Strongly connected: each pair of vertices is 
    connected with a directed path in each direction.

  A strongly connected component is a cyclic subgraph. 
  If there are no cycles (tree or DAG), 
  the strongly connected components
  are just individual vertices.
  """
  @type connectivity() :: :weak | :strong

  defguard is_conn(c) when c in [:weak, :strong]

  @typedoc """
  The kinds of search traversal.

  Depth First Search explores children before siblings.
  DFS follows a single chain forward as far as possible,
  before backtracking to the most recent ignored edges.

  Breadth First Search explores siblings before children.
  BFS can be thought of as stepping out in hops from the source.
  Each frontier is explored in sequence of increasing radius.

  Note that edges are unordered for any given vertex. 
  So the actual sequence of traversal within any 
  set of edges for a vertex (children, siblings) 
  is arbitrary.
  """
  @type traversality() :: :dfs | :bfs

  defguard is_trav(fs) when fs in [:dfs, :bfs]

  @typedoc """
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
  """
  @type vert_class() ::
          :isolated
          | :source
          | :sink
          | :linear
          | :self_isolated
          | :self_source
          | :self_sink
          | :self_linear
          | :complex

  # path ----------

  @typedoc """
  A _path_ in the graph is a non-empty sequence of adjacent vertices.
  There must be an edge for every successive pair of vertices in the path.

  A _simple_ path, which may represent a shortest path, 
  is non self-intersecting, so it cannot have a repeated vertex 

  A _ring_ is a closed path, 
  if the first and last vertices are the same,
  and a _simple ring_ if no other vertices are repeated.

  In an unweighted graph, the distance of the path 
  is one less than the length of the path list.

  A zero length path just contains a single vertex id.
  """
  @type path() :: [vert(), ...]

  defguard is_path(p) when is_list_nonempty(p) and is_vert(hd(p))

  @typedoc """
  In an unweighted graph, distance is a non-negative integer,
  counting the number of edges to be traversed.
  """
  @type distance() :: non_neg_integer()

  defguard is_distance(d) when is_int_nonneg(d)

  # components ----------

  @typedoc """
  A component is identified by its lowest vertex id.
  """
  @type comp_id() :: vert()

  @typedoc """
  A component edge is an edge between two components.

  The source and destination vertices are mapped to their component id,
  so the data format is the same as for a regular edge.
  """
  @type comp_edge() :: {comp_id(), comp_id()}

  @typedoc """
  A component is a weakly/strongly connected set of vertices.

  We choose the minimum vertex id in the component 
  as the component id.

  A component index is a map from the component id
  to the set of distinct vertices in the component.
  """
  @type components() :: Exa.Std.Mos.mos(comp_id(), vert())

  @typedoc """
  An index of the component id for every vertex.

  The index is the inverse of the components map.
  """
  @type component_index() :: %{vert() => comp_id()}

  @typedoc """
  A spanning forest is a sequence of directed rooted trees.
  The forest includes all vertices, but only a subset of the edges.

  The forest is represented as a map of vertices 
  to a list of their outgoing directed tree edges (MoL).

  Leaf vertices with no outgoing tree edges 
  do not have an entry in the forest.

  There is a special key `:forest` that 
  contains a list of the roots of the trees.
  """
  @type forest() :: Mol.mol(:forest | G.vert(), G.vert())

  defguard is_forest(f) when is_map(f) and is_map_key(f, :forest)

  # hash ----------

  @typedoc """
  A hash value for a graph object.

  The hash will be a SHA256 binary converted to an unsigned integer.
  """
  @type hash() :: non_neg_integer()
end
