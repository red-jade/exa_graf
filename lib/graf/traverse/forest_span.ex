alias Exa.Std.Mol
alias Exa.Graf.Traverse.Api.State

defmodule Exa.Graf.Traverse.ForestSpan do
  # dff: Map of Lists
  # chikdren: empty stack List of Lists 
  defstruct dff: Mol.new(), lol: [[]]
end

alias Exa.Graf.Traverse.ForestSpan

defimpl State, for: ForestSpan do
  defdelegate init_state(state, g), to: State.Any
  defdelegate test_node(state, g, i), to: State.Any

  def pre_component(%ForestSpan{dff: dff} = fs, _g, root) do
    # add new root to forest
    %ForestSpan{fs | dff: Mol.append(dff, :forest, root)}
  end

  def pre_branch(%ForestSpan{lol: lol} = fs, _g, _i0) do
    # push an empty list of children onto the stack
    %ForestSpan{fs | lol: [[] | lol]}
  end

  # leaf node, dff no-op default empty, push i into the parent child list
  def visit_leaf(%ForestSpan{dff: dff, lol: [is | lol]} = fs, _g, i) do
    %ForestSpan{fs | dff: dff, lol: [[i | is] | lol]}
  end

  # pop the stack, build the tree adjacency, push i into the parent child list
  def post_branch(%ForestSpan{dff: dff, lol: [js, is | lol]}, _g, i) do
    {Mol.set(dff, i, Enum.reverse(js)), [[i | is] | lol]}
  end

  def final_result(%ForestSpan{dff: dff}), do: dff
end
