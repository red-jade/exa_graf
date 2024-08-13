defmodule Exa.Graf.GrafTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  import Exa.Graf.Graf

  # add ----------

  test "add vert" do
    builder(
      fn tag -> new(tag, "vert") |> add(1) |> add(2) |> add(3) end,
      {:agra, "vert",
       {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
        %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "add verts" do
    builder(
      fn tag -> new(tag, "verts") |> add([1, 2, 3]) end,
      {:agra, "verts",
       {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
        %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "add range" do
    builder(
      fn tag -> new(tag, "range") |> add(1..3) end,
      {:agra, "range",
       {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
        %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "add edge" do
    builder(
      fn tag -> new(tag, "edge") |> add({1, 2}) |> add({2, 3}) |> add({1, 3}) end,
      {:agra, "edge",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "add edges" do
    builder(
      fn tag -> new(tag, "edges") |> add([{1, 2}, {2, 3}, {1, 3}]) end,
      {:agra, "edges",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "add adj" do
    builder(
      fn tag -> new(tag, "adj") |> add({1, [2, 3]}) |> add({2, [3]}) end,
      {:agra, "adj",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "add self loop" do
    builder(
      fn tag -> new(tag, "self loop") |> add(1) |> add({1, 1}) end,
      {:agra, "self_loop", {%{1 => MapSet.new([1])}, %{1 => MapSet.new([1])}}}
    )
  end

  test "add repeated vert" do
    builder(
      fn tag -> new(tag, "vert") |> add(1) |> add(1) |> add([3, 3]) end,
      {:agra, "vert",
       {%{1 => MapSet.new([]), 3 => MapSet.new([])}, %{1 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  test "add repeated edge" do
    builder(
      fn tag -> new(tag, "edge") |> add({1, 2}) |> add({1, 2}) |> add([{1, 3}, {1, 3}]) end,
      {:agra, "edge",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  # remove ----------

  defp tri(tag), do: build(tag, "tri", [{1, 2}, {2, 3}, {1, 3}])

  test "valid tri" do
    builder(
      &tri/1,
      {:agra, "tri",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
        %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "del vert" do
    builder(
      fn tag -> tri(tag) |> delete(1) end,
      {:agra, "tri",
       {%{2 => MapSet.new([]), 3 => MapSet.new([2])},
        %{2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "del edge" do
    builder(
      fn tag -> tri(tag) |> delete({1, 3}) end,
      {:agra, "tri",
       {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([2])},
        %{1 => MapSet.new([2]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "del adj" do
    builder(
      fn tag -> tri(tag) |> delete({1, [2, 3]}) end,
      {:agra, "tri",
       {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([2])},
        %{1 => MapSet.new([]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
    )
  end

  test "del list" do
    builder(
      fn tag -> tri(tag) |> delete([1, {2, 3}]) end,
      {:agra, "tri",
       {%{2 => MapSet.new([]), 3 => MapSet.new([])}, %{2 => MapSet.new([]), 3 => MapSet.new([])}}}
    )
  end

  defp builder(build, result) do
    for tag <- [:agra, :dig] do
      assert convert(build.(tag), :agra) == result
    end
  end
end
