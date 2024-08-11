defmodule Exa.Graf.AgraTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  import Exa.Graf.Agra

  # add ----------

  test "add vert" do
    g = "vert" |> new() |> add(1) |> add(2) |> add(3)

    assert g ==
             {:agra, "vert",
              {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
               %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
  end

  test "add verts" do
    g = "verts" |> new() |> add([1, 2, 3])

    assert g ==
             {:agra, "verts",
              {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
               %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
  end

  test "add range" do
    g = "range" |> new() |> add(1..3)

    assert g ==
             {:agra, "range",
              {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])},
               %{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
  end

  test "add edge" do
    g = "edge" |> new() |> add({1, 2}) |> add({2, 3}) |> add({1, 3})

    assert g ==
             {:agra, "edge",
              {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
               %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
  end

  test "add edges" do
    g = "edges" |> new() |> add([{1, 2}, {2, 3}, {1, 3}])

    assert g ==
             {:agra, "edges",
              {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
               %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
  end

  test "add adj" do
    g = "adj" |> new() |> add({1, [2, 3]}) |> add({2, [3]})

    assert g ==
             {:agra, "adj",
              {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
               %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
  end

  test "add self loop" do
    g = "self loop" |> new() |> add(1) |> add({1, 1})
    assert g == {:agra, "self_loop", {%{1 => MapSet.new([1])}, %{1 => MapSet.new([1])}}}
  end

  test "add repeated vert" do
    g = "vert" |> new() |> add(1) |> add(1) |> add([3, 3])

    assert g ==
             {:agra, "vert",
              {%{1 => MapSet.new([]), 3 => MapSet.new([])},
               %{1 => MapSet.new([]), 3 => MapSet.new([])}}}
  end

  test "add repeated edge" do
    g = "edge" |> new() |> add({1, 2}) |> add({1, 2}) |> add([{1, 3}, {1, 3}])

    assert g ==
             {:agra, "edge",
              {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1])},
               %{1 => MapSet.new([2, 3]), 2 => MapSet.new([]), 3 => MapSet.new([])}}}
  end

  # remove ----------

  @tri new("tri", [{1, 2}, {2, 3}, {1, 3}])

  test "valid tri" do
    assert @tri ==
             {:agra, "tri",
              {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([1, 2])},
               %{1 => MapSet.new([2, 3]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
  end

  test "del vert" do
    g = delete(@tri, 1)

    assert g ==
             {:agra, "tri",
              {%{2 => MapSet.new([]), 3 => MapSet.new([2])},
               %{2 => MapSet.new([3]), 3 => MapSet.new([])}}}
  end

  test "del edge" do
    g = delete(@tri, {1,3})

    assert g ==
             {:agra, "tri",
              {%{1 => MapSet.new([]), 2 => MapSet.new([1]), 3 => MapSet.new([2])},
               %{1 => MapSet.new([2]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
  end

  test "del adj" do
    g = delete(@tri, {1,[2,3]})

    assert g ==
             {:agra, "tri",
              {%{1 => MapSet.new([]), 2 => MapSet.new([]), 3 => MapSet.new([2])},
               %{1 => MapSet.new([]), 2 => MapSet.new([3]), 3 => MapSet.new([])}}}
  end

  test "del list" do
    g = delete(@tri, [1, {2,3}])

    assert g ==
             {:agra, "tri",
              {%{2 => MapSet.new([]), 3 => MapSet.new([])},
               %{2 => MapSet.new([]), 3 => MapSet.new([])}}}
  end
end
