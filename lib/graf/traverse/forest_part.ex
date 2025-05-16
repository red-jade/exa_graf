alias Exa.Std.Mos
alias Exa.Graf.Traverse.Api.State

defmodule Exa.Graf.Traverse.ForestPart do
  # current component root; MoS graph partition 
  defstruct root: nil, part: Mos.new()
end

alias Exa.Graf.Traverse.ForestPart

defimpl State, for: ForestPart do
  defdelegate init_state(state, g), to: State.Any
  defdelegate test_node(state, g, i), to: State.Any
  defdelegate post_branch(state, g, i), to: State.Any

  def pre_component(%ForestPart{} = fp, _g, root) do
    %ForestPart{fp | root: root}
  end

  def pre_branch(%ForestPart{root: root, part: part} = fp, _g, i) do
    %ForestPart{fp | part: Mos.add(part, root, i)}
  end

  def visit_leaf(%ForestPart{root: root, part: part} = fp, _g, i) do
    %ForestPart{fp | part: Mos.add(part, root, i)}
  end

  def final_result(%ForestPart{part: part}), do: part
end
