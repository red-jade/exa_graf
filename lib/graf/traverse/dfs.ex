defmodule Exa.Graf.Traverse.Dfs do
  @moduledoc """
  A graph traversal control implementation for Depth First Search (DFS)
  or DFS traversal of a Depth First Forest (DFF) of pure trees.
  """
  alias Exa.Graf.Types, as: G

  defmodule Dfs do
    alias Exa.Graf.Types, as: G

    defstruct stack: [],
              unvis: MapSet.new()

    @type t :: %__MODULE__{
            stack: G.verts(),
            unvis: MapSet.t()
          }
  end

  @doc "Create empty DFS data."
  @spec new() :: Dfs.t()
  def new(), do: %Dfs{}

  # --------
  # protocol
  # --------

  defimpl Exa.Graf.Traverse.Api.Control, for: Dfs do
    import Exa.Types
    alias Exa.Graf.Graf

    # common to BFS & DFS

    def init_data(%Dfs{} = data, g) do
      %Dfs{data | unvis: g |> Graf.verts() |> MapSet.new()}
    end

    def select_root(%Dfs{stack: [], unvis: unvis} = empty, nil) when is_set_empty(unvis) do
      {:empty, empty}
    end

    def select_root(%Dfs{stack: [], unvis: unvis} = dfs, nil) do
      stack = Enum.take(unvis, 1)
      {hd(stack), %Dfs{dfs | stack: stack}}
    end

    def select_root(%Dfs{stack: [], unvis: unvis} = dfs, i) when is_set_member(unvis, i) do
      {i, %Dfs{dfs | stack: [i]}}
    end

    def select_root(%Dfs{stack: []}, i) do
      raise ArgumentError, message: "Vertex #{i} not in graph"
    end

    # specific for DFS

    def pop_node(%Dfs{stack: stack, unvis: unvis}) do
      # dfs tests nodes as they come off the stack
      {i, {new_stack, new_unvis}} = taken(stack, unvis)
      {i, %Dfs{stack: new_stack, unvis: new_unvis}}
    end

    def pop_node(%Dfs{stack: []} = dfs) do
      {:empty, dfs}
    end

    def push_nodes(%Dfs{stack: stack, unvis: unvis}, frontier) do
      # only consider unvisited nodes on frontier
      fset = MapSet.intersection(unvis, frontier)
      type = if MapSet.size(fset) == 0, do: :leaf, else: :branch
      # dfs does not mark children as visited here
      # because dfs can loop back up from children to siblings
      # dfs pushes children at front of stack
      {type, %Dfs{stack: MapSet.to_list(fset) ++ stack, unvis: unvis}}
    end

    defp taken([i | stack], unvis) when not is_set_member(unvis, i), do: taken(stack, unvis)
    defp taken([i | stack], unvis), do: {i, {stack, MapSet.delete(unvis, i)}}
    defp taken([], unvis), do: {:empty, {[], unvis}}
  end
end
