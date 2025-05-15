# -------------------
# structure functions
# -------------------

defprotocol Exa.Graf.Traverse.Api.Control do
  alias Exa.Graf.Types, as: G

  @doc "Initialize the structural state."
  @spec init_data(Control.t(), G.graph()) :: Control.t()
  def init_data(gdata, g)

  @doc """
  Select or validate a root for initiating 
  a traversal of a component of the graph.

  The root may be proposed by the client, 
  or pass `nil` for the Control.t() to select 
  a new root from the graph data.
  """
  @spec select_root(Control.t(), G.vert() | nil) :: {G.vert() | :empty, Control.t()}
  def select_root(gdata, root)

  @doc ""
  @spec push_nodes(Control.t(), G.vset()) :: {:leaf | :branch, Control.t()}
  def push_nodes(gdata, vset)

  @doc """
  Pop the next node for traversal.
  Updat the graph data for further traversal.
  """
  @spec pop_node(Control.t()) :: {G.vert() | :empty, Control.t()}
  def pop_node(gdata)
end

# ---------------
# state functions
# ---------------

defprotocol Exa.Graf.Traverse.Api.State do
  @moduledoc """
  A protocol for graph traversal state.

    The callbacks are invoked as follows:
    - `init_state`
    - For each component:
      - `pre_component`
      - For each node:
        - `test_node` 
        - If a branch node:
          - `pre_branch`
          - Traverse children
          - `post_branch`
        - If a leaf node:
          - `visit_leaf`
    - `final_result`

    A branch node is one that has at least one nested child
    active in the traversal. 
    Adjacent nodes will be active
    if they are not filtered out by the traversal mechanism.
    For example, only visiting previously unvisted nodes.

    Default no-op implementations are provided for all functions.
    Only define the callbacks you need.
  """

  @fallback_to_any true

  alias Exa.Graf.Types, as: G

  @doc "Initialize the client state."
  @spec init_state(State.t(), G.graph()) :: State.t()
  def init_state(state, g)

  @doc "Process client state at start of a new component."
  @spec pre_component(State.t(), G.graph(), G.vert()) :: State.t()
  def pre_component(state, g, root)

  @doc """
  Test the client state to promptly stop the traversal.

  The options are to continue without modifying the state, 
  or halt traversal and return an end state.
  The end state will be passed to the `final_result` post process.
  """
  @spec test_node(State.t(), G.graph(), G.vert()) :: :cont | {:halt, State.t()}
  def test_node(state, g, i)

  @doc """
  Pre-process client state for a branch node
  before the traversal of children in a DFS.
  """
  @spec pre_branch(State.t(), G.graph(), G.vert()) :: State.t()
  def pre_branch(state, g, i)

  @doc """
  Process state for a node.
  """
  @spec visit_leaf(State.t(), G.graph(), G.vert()) :: State.t()
  def visit_leaf(state, g, i)

  @doc """
  Post-process client state for a branch node
  after the traversal of children in a DFS.
  """
  @spec post_branch(State.t(), G.graph(), G.vert()) :: State.t()
  def post_branch(state, g, i)

  @doc """
  Convert the final traversal state into a return value.

  Typically the result will be a simple reformatting, 
  such as reversing lists, or discarding completed counters.
  """
  @spec final_result(State.t()) :: any()
  def final_result(state)
end
