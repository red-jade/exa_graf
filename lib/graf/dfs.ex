defmodule Exa.Graf.Dfs do
  @moduledoc """
  Graph utilities based on Depth First Search (DFS)
  and the Depth First Foreast (DFF).

  Provide functions to generate the DFF
  and execute generalized traversals over the forest.
  """
  require Logger

  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Std.Mol

  alias Exa.Graf.Graf

  # -----------
  # local types
  # -----------

  # depth-first traversal data
  @typep dfs() :: {unvisited :: G.vset(), dff :: G.forest()}

  # ----------------
  # public functions
  # ----------------

  @doc """
  Build a spanning forest for the graph.
  A spanning forest is a sequence of directed rooted trees.
  The forest includes all vertices, but only a subset of the edges.

  Three classes of edges are excluded:
  - reverse: to ancestors within the same tree
  - forward: to non-ancestors on other branches within the same tree 
  - cross: from a later tree to an earlier tree

  By construction, there are no _forward cross_ edges
  from earlier trees to later trees.

  Self-loops do not affect the forest,
  because the source of the edge is already in the tree.

  The forest is represented as a map of vertices 
  to a list of their outgoing directed tree edges (MoL).
  Leaf vertices with no outgoing tree edges 
  do not have an entry in the forest.
  There is a special key `:forest` that 
  contains a list of the roots of trees.

  The spanning forest is created by depth-first traversal,
  so it is often called a _depth-first forest_ (dff).

  If the graph is a single weak/strong connected component,
  then the Depth First Forest is actually a single spanning tree.
  Use adjacency:
  - weak `:in_out`
  - strong `:out`

  The initial root vertex for the depth-first search may be specified
  as an argument, otherwise it is picked arbitrarily from the graph.
  """
  @spec spanning_forest(G.graph(), G.adjacency(), nil | G.vert()) :: G.forest()
  def spanning_forest(g, adjy \\ :out, root \\ nil)
      when is_graph(g) and is_adjacency(adjy) and (is_nil(root) or is_vert(root)) do
    vs = g |> Graf.verts() |> MapSet.new()

    if not is_nil(root) and not MapSet.member?(vs, root) do
      msg = "Root #{root} does not exist"
      Logger.error(msg)
      raise ArgumentError, message: msg
    end

    do_tree(g, adjy, {vs, Mol.new()}, root)
  end

  @spec do_tree(G.graph(), G.adjacency(), dfs(), nil | G.vert()) :: G.forest()
  defp do_tree(g, adjy, {vs, dff}, root) do
    i = if is_nil(root), do: vs |> Enum.take(1) |> hd(), else: root
    dff = Mol.append(dff, :forest, i)
    {new_vs, new_dff} = dfs = visit(g, adjy, i, {vs, dff}, MapSet.new())

    if MapSet.size(new_vs) == 0 do
      new_dff
    else
      do_tree(g, adjy, dfs, nil)
    end
  end

  @spec visit(G.graph(), G.adjacency(), G.vert(), dfs(), G.vset()) :: dfs()
  defp visit(g, adjy, i, {vs, dff}, path) do
    vs = MapSet.delete(vs, i)
    ipath = MapSet.put(path, i)
    neigh = Graf.neighborhood(g, i, adjy)

    {{new_vs, new_dff}, ichilds} =
      Enum.reduce(neigh, {{vs, dff}, []}, fn j, {{vs, _} = dfs, js} = acc ->
        if not MapSet.member?(vs, j) do
          acc
        else
          {visit(g, adjy, j, dfs, ipath), [j | js]}
        end
      end)

    {new_vs, Mol.set(new_dff, i, Enum.reverse(ichilds))}
  end

  # -----------------
  # traversal visitor
  # -----------------

  @typedoc """
  Function to initialize the traversal state.

  The function just returns a constant inital state.

  Default implementation is to return `nil`.
  """
  @type init_fun(s) :: (G.graph(), G.forest() -> s)

  @typedoc """
  Function invoked for a vertex in the forest.

  There are three situations where the function is invoked:
  - branch node: 
    - before (pre) traversal of children
    - after (post) traversal of children
  - leaf node: during traversal

  The path is the reverse list of ancestors
  from the current vertex to the root of the current tree.
  The current vertex is the head of the path.

  Default implementation passes through the state unchanged.
  """
  @type node_fun(s) :: (G.graph(), G.path(), s -> s)

  @typedoc """
  Function to convert the final traversal state to a result.

  Typically this will be a simple reformatting, 
  such as reversing lists, or discarding completed counters.

  Default implementation is to pass the state through unchanged.
  """
  @type final_fun(s, r) :: (s -> r)

  defmodule DefaultFuns do
    def pass_through(_, _, state), do: state
    def pass_through(state), do: state
    def nil_state(_, _), do: nil
  end

  defmodule ForestVisitor do
    import DefaultFuns

    defstruct init_state: &nil_state/2,
              pre_branch: &pass_through/3,
              post_branch: &pass_through/3,
              visit_leaf: &pass_through/3,
              final_result: &pass_through/1
  end

  @type forest_visitor(s, r) :: %ForestVisitor{
          init_state: init_fun(s),
          pre_branch: node_fun(s),
          post_branch: node_fun(s),
          visit_leaf: node_fun(s),
          final_result: final_fun(s, r)
        }

  defguardp is_forvis(v) when is_struct(v, ForestVisitor)

  @doc """
  Traverse the Depth First Forest
  by applying functions from a forest visitor.

  The optional graph is only used 
  as an argument to visitor callbacks.
  If it is not needed by the visitor,  it can be omitted.
  """
  @spec traverse(nil | G.graph(), G.forest(), forest_visitor(any(), any())) :: any()
  def traverse(g \\ nil, forest, vis)
      when (is_nil(g) or is_graph(g)) and is_forest(forest) and is_forvis(vis) do
    forest
    |> Mol.get(:forest)
    |> Enum.reduce(vis.init_state.(g, forest), fn root, state ->
      do_traverse(g, vis, forest, root, [], state)
    end)
    |> vis.final_result.()
  end

  defp do_traverse(g, vis, forest, i, path, state) do
    path = [i | path]

    case Mol.get(forest, i) do
      [] ->
        vis.visit_leaf.(g, path, state)

      children ->
        pre_state = vis.pre_branch.(g, path, state)

        post_state =
          Enum.reduce(children, pre_state, fn j, state ->
            do_traverse(g, vis, forest, j, path, state)
          end)

        vis.post_branch.(g, path, post_state)
    end
  end

  # ----------
  # traversals
  # ----------

  @doc "Print out an indented view of the traversal of the Depth First Forest."
  @spec dump(G.forest()) :: nil
  def dump(dff) when is_forest(dff) do
    traverse(
      dff,
      %Exa.Graf.Dfs.ForestVisitor{
        init_state: fn _, _ -> ["  "] end,
        pre_branch: fn _g, [i | _], indent ->
          IO.puts([indent, "#{i} pre"])
          ["  " | indent]
        end,
        post_branch: fn _g, [i | _], indent ->
          indent = tl(indent)
          IO.puts([indent, "#{i} post"])
          indent
        end,
        visit_leaf: fn _g, [i | _], indent ->
          IO.puts([indent, "#{i} leaf"])
          indent
        end
      }
    )
  end

  @doc "Get a pre-order traversal sequence of vertices."
  @spec preorder(G.forest()) :: G.verts()
  def preorder(dff) when is_forest(dff) do
    traverse(
      dff,
      %ForestVisitor{
        init_state: fn _, _ -> [] end,
        pre_branch: fn _g, [i | _], ord -> [i | ord] end,
        visit_leaf: fn _g, [i | _], ord -> [i | ord] end,
        final_result: fn ord -> Enum.reverse(ord) end
      }
    )
  end

  @doc "Get a post-order traversal sequence of vertices."
  @spec postorder(G.forest()) :: G.verts()
  def postorder(dff) when is_forest(dff) do
    traverse(
      dff,
      %ForestVisitor{
        init_state: fn _, _ -> [] end,
        post_branch: fn _g, [i | _], ord -> [i | ord] end,
        visit_leaf: fn _g, [i | _], ord -> [i | ord] end,
        final_result: fn ord -> Enum.reverse(ord) end
      }
    )
  end
end
