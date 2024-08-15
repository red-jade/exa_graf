defmodule Exa.Graf.Types do
  @moduledoc "Types and guards for agra graph format."

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Std.Mos, as: M

  # agra format ----------

  @typedoc "A vertex id is a positive integer."
  @type vert() :: pos_integer()

  defguard is_vert(i) when is_pos_int(i)

  @typedoc "List of vertices."
  @type verts() :: [vert()]

  @typedoc "A contiguous sequence of vertex ids."
  @type vseq() :: Range.t()

  @typedoc "A set of vertices."
  @type vset() :: MapSet.t(vert())

  @typedoc """
  A directed edge is an ordered pair of vertices.
  The vertices should be previously defined.
  """
  @type edge() :: {vert(), vert()}

  defguard is_edge(e)
           when is_fix_tuple(e, 2) and
                  is_vert(elem(e, 0)) and is_vert(elem(e, 1))

  @typedoc "List of edges."
  @type edges() :: [edge()]

  @typedoc "A single adjacency list."
  @type adj() :: {vert(), verts()}

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
  - out adjacency list 
  - list of all of the above
  """
  @type gelem() :: vert() | vseq() | edge() | adj() | glist()

  @typedoc "List of graph elements."
  @type glist() :: [gelem()]

  @typedoc "Graphs have a string name."
  @type gname() :: String.t()

  defguard is_gname(gname) when is_nonempty_string(gname)

  @typedoc """
  An agra graph data structure.

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
  @type agra() :: {:agra, gname(), adjmaps()}

  defguard is_agra(a) when is_tag_tuple(a, 3, :agra) and is_gname(elem(a, 1))

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
  @type gtype() :: :agra | :dig

  defguard is_gtype(t) when t in [:agra, :dig]

  @typedoc """
  A supertype for all graph types.

  A graphs must be a tagged 3-tuple, with name and specific data.
  """
  @type graph() :: agra() | dig()

  defguard is_graph(g) when is_fix_tuple(g, 3) and is_gtype(elem(g, 0))

  # counts, neighborhoods, adjacency and degree ----------

  @typedoc """
  Degree is the count of edges for a vertex.

  Depending on the adjacency type:
  - `:in`: number of incoming edges (for dst vertex)
  - `:out`: number of outgoing edges (for src vertex)
  - `:inout`: combined degree in+out for all incident edges
  """
  @type degree() :: E.count()

  @typedoc """
  Type of adjacency neighborhood:
  - `in` incoming edges and upstream neighbors
  - `out` outgoing edges and downstream neighbors
  - `inout` combined incident edges and all adjacent neighbors
  """
  @type adjacency() :: :in | :out | :inout

  @typedoc """
  Classify a vertex.

  Values are:
  - `:isolated` no incident edges
  - `:source` only outgoing edges
  - `:sink` only incoming edges
  - `:self` isolated with only a self loop
  - `:linear` exactly 1 incoming and 1 outgoing edge, but not a self-loop
  - `:complex` both incoming and outgoing edges, 
     with one direction having at least 2 edges, 
     including contribution of self-loops
  """
  @type vert_class() :: :isolated | :source | :sink | :self | :linear | :complex

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
  to the list of vertices for the component.
  """
  @type components() :: %{vert() => verts()}

  # hash ----------

  @typedoc "A hash value for a graph object."
  @type ghash() :: binary()
end
