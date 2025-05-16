defmodule Exa.Graf.Contract do
  @moduledoc """
  Contract edges and nodes of a graph.
  """
  use Exa.Constants
  use Exa.Graf.Constants

  import Exa.Types

  import Exa.Graf.Types
  alias Exa.Graf.Types, as: G

  alias Exa.Graf.Graf

  @doc """
  Contract an edge.

  Merge the two nodes at the ends of an edge.
  The remaining node will have the minimum of the two vertex ids.
  The min node will have edges added 
  for all in/out neighbors of the max node.
  Any duplicated edges will be ignored.
  The max node and all its edges will be deleted.

  If the target edge is a self-edge,
  then it will be deleted,
  but no other changes are made.

  If the max node had a self-edge, 
  a self-edge will be added to the min node.

  If the target edge does not exist, there is no effect,
  and the result will be the same as the input.
  Otherwise, the input graph will be modified.
  """
  @spec edge(G.graph(), G.edge()) :: G.graph()
  def edge(g, {src, dst} = e) do
    cond do
      not Graf.edge?(g, e) ->
        g

      src == dst ->
        Graf.delete(g, e)

      true ->
        {i, j} = if src < dst, do: e, else: {dst, src}
        {ins, self, outs} = Graf.neighborhood(g, j, :in_self_out)

        # transfer any self-edge 
        g = if is_nil(self), do: g, else: g |> Graf.add({i, i}) |> Graf.delete({j, j})

        # transfer inward edges 
        # transfer outward edges
        # deleting the node deletes all its edges
        g
        |> Graf.add(Enum.map(ins, &{&1, i}))
        |> Graf.add(Enum.map(outs, &{i, &1}))
        |> Graf.delete(j)
    end
  end

  @doc """
  Contract a collection of distinct vertices onto a single vertex.
  The vertices do not have to be linked by edges.

  The remaining vertex will have the minimum of the vertex ids.

  The minimum node will have edges added 
  for all external neighbors of the node group,
  plus the transfer of any self-edge.

  The remainder of the node group and all its edges will be deleted.

  Any duplicated edges will be ignored.
  Any repeated vertices will have no effect.

  If only 0 or 1 target vertices exist, there is no effect,
  and the result will be the same as the input.
  Otherwise, the input graph will be modified.
  """
  @spec nodes(G.graph(), G.verts() | G.vset()) :: G.graph()
  def nodes(g, verts) do
    # filter non-existent/duplicate vertices and get min vertex value
    {imin, vset} =
      Enum.reduce(verts, {nil, @empty_set}, fn i, {imin, vset} = acc ->
        if Graf.vert?(g, i), do: {minil(imin, i), MapSet.put(vset, i)}, else: acc
      end)

    if MapSet.size(vset) < 2 do
      g
    else
      vset
      |> MapSet.delete(imin)
      |> Enum.reduce(g, fn j, g ->
        {jins, jself, jout} = Graf.neighborhood(g, j, :in_self_out)

        g
        |> add_self(imin, jself)
        |> Graf.add(jins |> MapSet.difference(vset) |> Enum.map(&{&1, imin}))
        |> Graf.add(jout |> MapSet.difference(vset) |> Enum.map(&{imin, &1}))
        |> Graf.delete(j)
      end)
    end
  end

  defp add_self(g, _i, nil), do: g
  defp add_self(g, i, _j), do: Graf.add(g, {i, i})

  defp minil(nil, k), do: k
  defp minil(i, k), do: min(i, k)

  @doc """
  Contract a linear or bilinear node.

  Contracting linear nodes does not change the 
  topological structure of the graph.

  The topologies of two graphs can be compared 
  by contracting all linear nodes and testing for isomorphism.
  If the two contractions are isomorphic, 
  then the original graphs are _homeomorphic_ (topologically equivalent).

  ### Linear Nodes

  A linear node has exactly one incoming edge, 
  one outgoing edge and no self-loop.

  The node is removed and the two edges are replaced by a
  single edge from the incoming neighbor to the outgoing neighbor.

  There are two additional constraints on the new edge:
  - not a duplicate of an existing edge
  - not a self-loop

  If the neighbors are the same node, 
  no self-loop is created.

  So, if the input graph is simple (no self-loops)
  then the output graph is also simple.

  ### Bilinear Nodes

  A similar process is applied to bilinear nodes.

  A bilinear node has exactly two incoming edges,
  two outgoing edges and no self-loop.

  The incoming and outgoing neighbors must be the same pair of nodes,
  which means the target node has bidirectional links to two distinct neighbors.

  Note the two nodes must be distinct, 
  because there cannot be two existing self-loops.

  The node is removed and the four edges are replaced by 
  a bidirectional pair of edges directly between the two neighbors,
  only if neither already exists.

  Contracting bilinear nodes means that undirected graphs modelled 
  with bidirectional edges can also be contracted in the expected way.
  """
  @spec linear(G.graph(), G.vert()) :: G.graph() | {:error, any()}
  def linear(g, i) when is_graph(g) and is_vert(i) do
    case Graf.neighborhood(g, i, :in_self_out) do
      {:error, _} = err ->
        err

      {_ins, ^i, _outs} ->
        {:error, "Self loop"}

      {ins, nil, outs} ->
        case {set_size(ins), set_size(outs)} do
          {1, 1} ->
            [j] = MapSet.to_list(ins)
            [k] = MapSet.to_list(outs)

            cond do
              j == k -> {:error, "Creates self-loop"}
              Graf.edge?(g, {j, k}) -> {:error, "Edge exists"}
              true -> g |> Graf.delete(i) |> Graf.add({j, k})
            end

          {2, 2} ->
            [j, k] = inlist = ins |> MapSet.to_list() |> Enum.sort()
            outlist = outs |> MapSet.to_list() |> Enum.sort()

            cond do
              inlist != outlist -> {:error, "Not linear"}
              Graf.edge?(g, {j, k}) -> {:error, "Edge exists"}
              Graf.edge?(g, {k, j}) -> {:error, "Edge exists"}
              true -> g |> Graf.delete(i) |> Graf.add([{j, k}, {k, j}])
            end

          _ ->
            {:error, "Not linear"}
        end
    end
  end

  @doc """
  Contract all linear nodes.

  Contracting linear nodes preserves topological structure,
  so it is a _homeomorphism_ (topological equivalence).

  See `contract_linear/2`.
  """
  @spec linears(G.graph()) :: G.graph()
  def linears(g) when is_graph(g) do
    name = Graf.name(g)

    g
    |> Graf.verts()
    |> Enum.reduce({g, false}, fn i, {g, changed?} ->
      case linear(g, i) do
        {:error, _} -> {g, changed?}
        new_g -> {new_g, true}
      end
    end)
    |> then(fn
      {g, false} -> g
      {new_g, true} -> Graf.rename(new_g, "#{name}_con")
    end)
  end
end
