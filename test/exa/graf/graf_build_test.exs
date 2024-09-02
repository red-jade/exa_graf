defmodule Exa.Graf.GrafBuildTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Std.Histo1D
  alias Exa.Std.Histo2D

  alias Exa.Graf.Graf
  alias Exa.Graf.GrafBuild
  alias Exa.Graf.DotRender

  @n 10

  @in_dir Path.join(["test", "input", "graf", "agr"])

  @out_dir Path.join(["test", "output", "graf", "agr"])

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_agr)

  # defp out_file(name), do: Exa.File.join(@out_dir, name, @filetype_agr)

  test "empty" do
    for tag <- [:agra, :dig] do
      g = Graf.new(tag, "empty")
      render(g, {[], [], [], []})
      assert 0 == Graf.nvert(g)
      assert 0 == Graf.nedge(g)
      assert not Graf.connected?(g)
      assert 0 == Graf.ncomp(g)
      assert %{} == Graf.components(g)
    end
  end

  test "dust" do
    h1in = [10]
    h1out = [10]
    h1inout = [10]
    h2inout = [{{0, 0}, 10}]
    graphs = grafy(&GrafBuild.dust/2, {h1in, h1out, h1inout, h2inout})

    for g <- graphs do
      {1, 10} = Graf.verts_minmax(g)
      comps = Graf.components(g)
      assert not Graf.connected?(g)
      assert MapSet.new([1]) == Graf.reachable(g, 1)
      assert MapSet.new([5]) == Graf.reachable(g, 5)
      assert 10 == Graf.ncomp(g)
      assert Enum.reduce(1..10, %{}, &Map.put(&2, &1, [&1])) == comps
    end
  end

  test "line" do
    h1in = [1, 9]
    h1out = [1, 9]
    h1inout = [0, 2, 8]
    h2inout = [{{0, 1}, 1}, {{1, 0}, 1}, {{1, 1}, 8}]
    graphs = grafy(&GrafBuild.line/2, {h1in, h1out, h1inout, h2inout})

    for g <- graphs do
      comps = Graf.components(g)
      assert Graf.connected?(g)
      assert Graf.tree?(g)
      assert :source == Graf.classify(g, 1)
      assert :linear == Graf.classify(g, 2)
      assert :sink == Graf.classify(g, 10)
      assert MapSet.new(1..10) == Graf.reachable(g, 1)
      assert MapSet.new(5..10) == Graf.reachable(g, 5)
      assert 1 == map_size(comps)
      assert %{1 => Range.to_list(1..10)} == comps
    end
  end

  test "ring" do
    h1in = [0, 10]
    h1out = [0, 10]
    h1inout = [0, 0, 10]
    h2inout = [{{1, 1}, 10}]
    graphs = grafy(&GrafBuild.ring/2, {h1in, h1out, h1inout, h2inout})

    for g <- graphs do
      comps = Graf.components(g)
      assert Graf.connected?(g)
      assert not Graf.tree?(g)
      assert :linear == Graf.classify(g, 1)
      assert MapSet.new(1..10) == Graf.reachable(g, 1)
      assert MapSet.new(1..10) == Graf.reachable(g, 5)
      assert %{1 => Range.to_list(1..10)} == comps
    end
  end

  test "fan_in" do
    h1in = [9, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    h1out = [1, 9]
    h1inout = [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    h2inout = [{{0, 1}, 9}, {{9, 0}, 1}]
    grafy(&GrafBuild.fan_in/2, {h1in, h1out, h1inout, h2inout})
  end

  test "fan_out" do
    h1in = [1, 9]
    h1out = [9, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    h1inout = [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    h2inout = [{{0, 9}, 1}, {{1, 0}, 9}]
    grafy(&GrafBuild.fan_out/2, {h1in, h1out, h1inout, h2inout})
  end

  test "fan in/out iso" do
    fin = GrafBuild.fan_in(:agra, @n)
    fout = GrafBuild.fan_out(:agra, @n)
    assert :undecided == Graf.isomorphic?(fin, fin)
    assert :undecided == Graf.isomorphic?(fout, fout)
    assert false == Graf.isomorphic?(fin, fout)
  end

  test "wheel" do
    h1in = [1, 0, 9]
    h1out = [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    h1inout = [0, 0, 0, 9, 0, 0, 0, 0, 0, 1]
    h2inout = [{{0, 9}, 1}, {{2, 1}, 9}]
    graphs = grafy(&GrafBuild.wheel/2, {h1in, h1out, h1inout, h2inout})

    for g <- graphs do
      comps = Graf.components(g)
      assert Graf.connected?(g)
      assert not Graf.tree?(g)
      assert MapSet.new(1..10) == Graf.reachable(g, 1)
      assert MapSet.new(2..10) == Graf.reachable(g, 2)
      assert 1 = Graf.ncomp(g)
      assert %{1 => Range.to_list(1..10)} == comps
    end
  end

  test "clique" do
    h1in = [0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    h1out = [0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    h1inout = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    h2inout = [{{9, 9}, 10}]
    graphs = grafy(&GrafBuild.clique/2, {h1in, h1out, h1inout, h2inout})

    for g <- graphs do
      assert @n * (@n - 1) == Graf.nedge(g)
      comps = Graf.components(g)
      assert Graf.connected?(g)
      assert not Graf.tree?(g)
      assert :complex == Graf.classify(g, 1)
      assert :complex == Graf.classify(g, 5)

      assert MapSet.new(1..10) == Graf.reachable(g, 1)
      assert MapSet.new(1..10) == Graf.reachable(g, 5)
      assert 1 = Graf.ncomp(g)
      assert %{1 => Range.to_list(1..10)} == comps

      h = Graf.degree_histo2d(g)
      assert h == %{{9, 9} => 10}
      assert Histo2D.homogeneous(h)
    end
  end

  test "grid2d" do
    h1in = [0, 0, 4, 6, 2]
    h1out = [0, 0, 4, 6, 2]
    h1inout = [0, 0, 0, 0, 4, 0, 6, 0, 2]
    h2inout = [{{2, 2}, 4}, {{3, 3}, 6}, {{4, 4}, 2}]

    graphs =
      Enum.map([:agra, :dig], fn tag ->
        g = GrafBuild.grid2d(tag, 4, 3)
        render(g, {h1in, h1out, h1inout, h2inout})
        g
      end)

    for g <- graphs do
      assert Graf.connected?(g)
      assert not Graf.tree?(g)
      assert MapSet.new(1..12) == Graf.reachable(g, 1)
      assert MapSet.new(1..12) == Graf.reachable(g, 7)
      assert %{1 => Range.to_list(1..12)} == Graf.components(g)
    end
  end

  test "random" do
    for tag <- [:agra, :dig] do
      g = GrafBuild.random(tag, @n, 2 * @n)
      render(g)
      assert @n == Graf.nvert(g)
      assert 2 * @n == Graf.nedge(g)
    end
  end

  # read/write AGR format

  test "to file" do
    path = GrafBuild.line(:agra, @n) |> Graf.to_agra_file(@out_dir)
    assert path == Path.join([@out_dir, "line_#{@n}.agr"])
  end

  test "from file" do
    line = "line_#{@n}" |> in_file() |> Graf.from_agra_file()
    assert Graf.equal?(line, GrafBuild.line(:agra, @n))
  end

  # -----------------
  # private utilities
  # -----------------

  defp grafy(fun_new, result) do
    # return both graphs
    Enum.map([:agra, :dig], fn tag ->
      g = fun_new.(tag, @n)
      render(g, result)
      g
    end)
  end

  defp render(g, result \\ nil) do
    h1in = g |> Graf.degree_histo1d(:in) |> Histo1D.to_list()
    h1out = g |> Graf.degree_histo1d(:out) |> Histo1D.to_list()
    h1inout = g |> Graf.degree_histo1d(:inout) |> Histo1D.to_list()
    h2inout = g |> Graf.degree_histo2d() |> Histo2D.to_list()

    case Graf.to_agra_file(g, @out_dir) do
      {:error, msg} ->
        raise msg

      dotfile ->
        Enum.each([:png, :svg], fn fmt ->
          DotRender.render_dot(dotfile, fmt, @out_dir)
        end)

        if not is_nil(result) do
          assert result == {h1in, h1out, h1inout, h2inout}
        end
    end
  end
end
