alias Exa.Graf.Traverse.Api.State

defmodule Exa.Graf.Traverse.DffPost do
  defstruct path: [], order: []
end

alias Exa.Graf.Traverse.DffPost

defimpl State, for: DffPost do
  defdelegate init_state(state, g), to: State.Any
  defdelegate test_node(state, g, i), to: State.Any
  defdelegate pre_component(state, g, root), to: State.Any

  def pre_branch(%DffPost{path: path} = pre, _g, i) do
    %DffPost{pre | path: [i | path]}
  end

  def visit_leaf(%DffPost{order: ord} = pre, _g, i) do
    %DffPost{pre | order: [i | ord]}
  end

  def post_branch(%DffPost{path: path, order: ord} = pre, _g, i) do
    %DffPost{pre | path: tl(path), order: [i | ord]}
  end

  def final_result(%DffPost{order: ord}), do: Enum.reverse(ord)
end
