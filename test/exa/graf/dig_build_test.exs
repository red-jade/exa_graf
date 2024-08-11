defmodule Exa.Graf.DigBuildTest do
  use ExUnit.Case

  alias Exa.Std.Histo1D
  alias Exa.Std.Histo2D

  alias Exa.Graf.Dig
  alias Exa.Graf.DigBuild
  alias Exa.Graf.DotRender

  @n 10

  @dig_out_dir Path.join(["test", "output", "graf", "dig"])

  test "dig dust" do
    {h1in, h1out, h1inout, h2inout} = digger(&DigBuild.dust/1)
    assert h1in == [10]
    assert h1out == [10]
    assert h1inout == [10]
    assert h2inout == [{{0, 0}, 10}]
  end

  test "dig line   " do
    {h1in, h1out, h1inout, h2inout} = digger(&DigBuild.line/1)
    assert h1in == [1, 9]
    assert h1out == [1, 9]
    assert h1inout == [0, 2, 8]
    assert h2inout == [{{0, 1}, 1}, {{1, 0}, 1}, {{1, 1}, 8}]
  end

  test "dig ring   " do
    {h1in, h1out, h1inout, h2inout} = digger(&DigBuild.ring/1)
    assert h1in == [0, 10]
    assert h1out == [0, 10]
    assert h1inout == [0, 0, 10]
    assert h2inout == [{{1, 1}, 10}]
  end

  test "dig fan_in " do
    {h1in, h1out, h1inout, h2inout} = digger(&DigBuild.fan_in/1)
    assert h1in == [9, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h1out == [1, 9]
    assert h1inout == [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h2inout == [{{0, 1}, 9}, {{9, 0}, 1}]
  end

  test "dig fan_out" do
    {h1in, h1out, h1inout, h2inout} = digger(&DigBuild.fan_out/1)
    assert h1in == [1, 9]
    assert h1out == [9, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h1inout == [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h2inout == [{{0, 9}, 1}, {{1, 0}, 9}]
  end

  test "dig wheel  " do
    {h1in, h1out, h1inout, h2inout} = digger(&DigBuild.wheel/1)
    assert h1in == [1, 0, 9]
    assert h1out == [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h1inout == [0, 0, 0, 9, 0, 0, 0, 0, 0, 1]
    assert h2inout == [{{0, 9}, 1}, {{2, 1}, 9}]
  end

  test "dig clique " do
    {h1in, h1out, h1inout, h2inout} = digger(&DigBuild.clique/1)
    assert h1in == [0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    assert h1out == [0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    assert h1inout == [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    assert h2inout == [{{9, 9}, 10}]
  end

  test "dig grid2d" do
    {h1in, h1out, h1inout, h2inout} = DigBuild.grid2d(4, 3) |> render()
    assert h1in == [0, 0, 4, 6, 2]
    assert h1out == [0, 0, 4, 6, 2]
    assert h1inout == [0, 0, 0, 0, 4, 0, 6, 0, 2]
    assert h2inout == [{{2, 2}, 4}, {{3, 3}, 6}, {{4, 4}, 2}]
  end

  test "dig random" do
    g = DigBuild.random(@n, 2 * @n)
    assert @n == Dig.nvert(g)
    assert 2 * @n == Dig.nedge(g)
    render(g)
  end

  # -----------------
  # private utilities
  # -----------------

  defp digger(fun_new), do: render(fun_new.(@n))

  defp render(g) do
    h1in = g |> Dig.degree_histo1d(:in) |> Histo1D.to_list()
    h1out = g |> Dig.degree_histo1d(:out) |> Histo1D.to_list()
    h1inout = g |> Dig.degree_histo1d(:inout) |> Histo1D.to_list()
    h2inout = g |> Dig.degree_histo2d() |> Histo2D.to_list()

    {dotfile, _dot} = Dig.to_dot_file(g, @dig_out_dir)

    Enum.each([:png, :svg], fn fmt ->
      DotRender.render_dot(dotfile, fmt, @dig_out_dir)
    end)

    {h1in, h1out, h1inout, h2inout}
  end
end
