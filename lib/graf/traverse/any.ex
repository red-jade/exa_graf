# default no-op implementation for traversal state

defimpl Exa.Graf.Traverse.Api.State, for: Any do
  def init_state(state, _g), do: state

  def pre_component(state, _g, _root), do: state

  def test_node(_state, _g, _i), do: :cont

  def pre_branch(state, _g, _i), do: state

  def visit_leaf(state, _g, _i), do: state

  def post_branch(state, _g, _i), do: state

  def final_result(state), do: state
end
