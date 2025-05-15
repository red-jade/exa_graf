defmodule Exa.Graf.Traverse.Bfs do
  @moduledoc """
  A graph traversal control implementation for Breadth First Search (BFS).
  """
  alias Exa.Graf.Types, as: G

  defmodule Bfs do
    alias Exa.Graf.Types, as: G

    defstruct queue: [],
              unvis: MapSet.new()

    @type t :: %__MODULE__{
            queue: G.verts(),
            unvis: MapSet.t()
          }
  end

  @doc "Create empty BFS data."
  @spec new() :: Bfs.t()
  def new(), do: %Bfs{}

  # --------
  # protocol
  # --------

  defimpl Exa.Graf.Traverse.Api.Control, for: Bfs do
    import Exa.Types
    alias Exa.Graf.Graf

    # common to BFS & DFS

    def init_data(%Bfs{} = data, g) do
      %Bfs{data | unvis: g |> Graf.verts() |> MapSet.new()}
    end

    def select_root(%Bfs{queue: [], unvis: unvis} = empty, nil) when is_set_empty(unvis) do
      {:empty, empty}
    end

    def select_root(%Bfs{queue: [], unvis: unvis} = bfs, nil) do
      queue = Enum.take(unvis, 1)
      {hd(queue), %Bfs{bfs | queue: queue}}
    end

    def select_root(%Bfs{queue: [], unvis: unvis} = bfs, i) when is_set_member(unvis, i) do
      {i, %Bfs{bfs | queue: [i]}}
    end

    def select_root(%Bfs{queue: []}, i) do
      raise ArgumentError, message: "Vertex #{i} not in graph"
    end

    # specific to DFS

    def pop_node(%Bfs{queue: [i | queue], unvis: unvis}) do
      # hack - delete, but only the root is not already marked as visited
      {i, %Bfs{queue: queue, unvis: MapSet.delete(unvis, i)}}
    end

    def pop_node(%Bfs{queue: []} = bfs) do
      {:empty, bfs}
    end

    def push_nodes(%Bfs{queue: queue, unvis: unvis}, frontier) do
      # only consider unvisited nodes on frontier
      fset = MapSet.intersection(unvis, frontier)
      type = if MapSet.size(fset) == 0, do: :leaf, else: :branch
      # bfs marks children as visited here
      # because they must block loop back up from children
      new_unvis = MapSet.difference(unvis, fset)
      # bfs appends children to end of queue
      new_queue = queue ++ MapSet.to_list(fset)

      {type, %Bfs{queue: new_queue, unvis: new_unvis}}
    end
  end
end
