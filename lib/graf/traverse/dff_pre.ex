alias Exa.Graf.Traverse.Api.State

defmodule Exa.Graf.Traverse.DffPre do
  defstruct path: [], order: []
end

alias Exa.Graf.Traverse.DffPre

defimpl State, for: DffPre do
  defdelegate init_state(state, g), to: State.Any
  defdelegate test_node(state, g, i), to: State.Any
  defdelegate pre_component(state, g, root), to: State.Any

  def pre_branch(%DffPre{path: path, order: ord} = pre, _g, i) do
    %DffPre{pre | path: [i | path], order: [i | ord]}
  end

  def visit_leaf(%DffPre{order: ord} = pre, _g, i) do
    %DffPre{pre | order: [i | ord]}
  end

  def post_branch(%DffPre{path: path} = pre, _g, _i) do
    %DffPre{pre | path: tl(path)}
  end

  def final_result(%DffPre{order: ord}), do: Enum.reverse(ord)
end
