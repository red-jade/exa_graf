alias Exa.Graf.Traverse.Api.State

defmodule Pre do
  defstruct seq: []
end

defimpl State, for: Pre do
  defdelegate init_state(state, g), to: State.Any
  defdelegate test_node(state, g, i), to: State.Any
  defdelegate pre_component(state, g, root), to: State.Any
  defdelegate post_branch(state, g, root), to: State.Any

  def pre_branch(%Pre{seq: seq} = s, _g, i), do: %Pre{s | seq: [i | seq]}
  def visit_leaf(%Pre{seq: seq} = s, _g, i), do: %Pre{s | seq: [i | seq]}
  def final_result(%Pre{seq: seq}), do: Enum.reverse(seq)
end
