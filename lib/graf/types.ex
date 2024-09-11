defmodule Exa.Graf.Types do
  @moduledoc "Types and guards for graphs."

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Std.Mos, as: M

  # graf format ----------

  @typedoc "A vertex id is a positive integer."
  @type vert() :: pos_integer()

  defguard is_vert(i) when is_pos_int(i)

  @typedoc "List of vertices."
  @type verts() :: [vert()]

  @typedoc "A contiguous sequence of vertex ids."
  @type vseq() :: Range.t()

  @typedoc "A set of vertices."
  @type vset() :: MapSet.t(vert())

  @typedoc "A directed edge is an ordered pair of vertices."
  @type edge() :: {vert(), vert()}

  defguard is_edge(e)
           when is_fix_tuple(e, 2) and
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
  @type adjmap() :: M.mos(vert(), vert())

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

  defguard is_gname(gname) when is_nonempty_string(gname)

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

  defguard is_adj(a) when is_tag_tuple(a, 3, :adj) and is_gname(elem(a, 1))

  @typedoc """
  A named Erlang digraph.

  Graph elements are stored in ETS.

  The graph may be `:cyclic` or `:acyclic`.

  Self-loops are allowed for cyclic graphs.

  Repeated edges are not supported.
  """
  @type dig() :: {:dig, gname(), :digraph.graph()}

  defguard is_dig(g) when is_tag_tuple(g, 3, :dig)

  @typedoc "The set of tags for graph tuple types."
  @type gtype() :: :adj | :dig

  defguard is_gtype(t) when t in [:adj, :dig]

  @typedoc """
  A supertype for all graph types.

  A graphs must be a tagged 3-tuple, with name and specific data.
  """
  @type graph() :: adj() | dig()

  defguard is_graph(g) when is_fix_tuple(g, 3) and is_gtype(elem(g, 0))

  # counts, neighborhoods, adjacency and degree ----------

  @typedoc """
  Degree is the count of edges for a vertex.

  Depending on the adjacency type, 
  the degree is a scalar, pair or triple:
  - `:in` number of incoming edges
  - `:out` number of outgoing edges
  - `:in_out` degrees of incoming and outgoing edges
  - `:in_self_out` degrees of incoming edges, self-loop, 
     and outgoing edges

  The counts for `:in`, `:out` and `in_out` 
  will include the vertex itself if it has a self loop. 
  The _in/out_ counts for `:in_self_out`
  do not include any contribution from self-loop.
  """
  @type degree() :: E.count()

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

  @typedoc """
  Cyclicity property for the whole graph:
  - `:cyclic` general directed graph, allow cycles and self-loops
  - `:acyclic` _Directed Acyclic Graph_ (DAG), no cycles or self-loops
  """
  @type cyclicity() :: :cyclic | :acyclic

  defguard is_cyc(cyc) when cyc in [:cyclic, :acyclic]

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

  defguard is_path(p) when is_nonempty_list(p) and is_vert(hd(p))

  @typedoc """
  In an unweighted graph, distance is a non-negative integer,
  counting the number of edges to be traversed.
  """
  @type distance() :: non_neg_integer()

  defguard is_distance(d) when is_nonneg_int(d)

  # components ----------

  @typedoc """
  A component is a weakly/strongly connected set of vertices.

  We choose the minimum vertex id in the component 
  as the component id.

  A component index is a map from the component id
  to the list of distinct vertices in the component.

  See also:
  - `Exa.Std.Mol.sort/1` to sort the component ids
  - `Exa.Std.Mos.from_mol/1` to convert to vertex sets
  """
  @type components() :: Exa.Std.Mol.mol(vert(), vert())

  # hash ----------

  @typedoc """
  A hash value for a graph object.

  The hash will be a SHA256 binary converted to an unsigned integer.
  """
  @type hash() :: non_neg_integer()
end
