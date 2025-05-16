alias Exa.Graf.Traverse.Api.State

defmodule Exa.Graf.Traverse.DffDump do
  defstruct indent: []
end

alias Exa.Graf.Traverse.DffDump

defimpl State, for: DffDump do
  def init_state(dump, _g), do: %DffDump{dump | indent: ["  "]}
  defdelegate test_node(state, g, i), to: State.Any
  defdelegate pre_component(state, g, root), to: State.Any
  defdelegate final_result(state), to: State.Any

  def pre_branch(%DffDump{indent: indent} = dump, _g, i) do
    IO.puts([indent, "#{i} pre"])
    %DffDump{dump | indent: ["  " | indent]}
  end

  def visit_leaf(%DffDump{indent: indent} = dump, _g, i) do
    IO.puts([indent, "#{i} leaf"])
    dump
  end

  def post_branch(%DffDump{indent: indent} = dump, _g, i) do
    indent = tl(indent)
    IO.puts([indent, "#{i} post"])
    %DffDump{dump | indent: indent}
  end
end
