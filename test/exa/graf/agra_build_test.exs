defmodule Exa.Graf.AgraBuildTest do
  use ExUnit.Case

  use Exa.Graf.Constants

  alias Exa.Std.Histo1D
  alias Exa.Std.Histo2D

  alias Exa.Graf.Agra
  alias Exa.Graf.AgraBuild
  alias Exa.Graf.DotRender

  @n 10

  @in_dir Path.join(["test", "input", "graf", "agr"])

  @out_dir Path.join(["test", "output", "graf", "agr"])

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_agr)

  #defp out_file(name), do: Exa.File.join(@out_dir, name, @filetype_agr)

  test "agra dust" do
    {h1in, h1out, h1inout, h2inout} = agrar(&AgraBuild.dust/1)
    assert h1in == [10]
    assert h1out == [10]
    assert h1inout == [10]
    assert h2inout == [{{0, 0}, 10}]
  end

  test "agra line   " do
    {h1in, h1out, h1inout, h2inout} = agrar(&AgraBuild.line/1)
    assert h1in == [1, 9]
    assert h1out == [1, 9]
    assert h1inout == [0, 2, 8]
    assert h2inout == [{{0, 1}, 1}, {{1, 0}, 1}, {{1, 1}, 8}]
  end

  test "agra ring   " do
    {h1in, h1out, h1inout, h2inout} = agrar(&AgraBuild.ring/1)
    assert h1in == [0, 10]
    assert h1out == [0, 10]
    assert h1inout == [0, 0, 10]
    assert h2inout == [{{1, 1}, 10}]
  end

  test "agra fan_in " do
    {h1in, h1out, h1inout, h2inout} = agrar(&AgraBuild.fan_in/1)
    assert h1in == [9, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h1out == [1, 9]
    assert h1inout == [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h2inout == [{{0, 1}, 9}, {{9, 0}, 1}]
  end

  test "agra fan_out" do
    {h1in, h1out, h1inout, h2inout} = agrar(&AgraBuild.fan_out/1)
    assert h1in == [1, 9]
    assert h1out == [9, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h1inout == [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h2inout == [{{0, 9}, 1}, {{1, 0}, 9}]
  end

  test "agra fan in/out iso" do
    fin = AgraBuild.fan_in(@n)
    fout = AgraBuild.fan_out(@n)
    assert :undecided == Agra.isomorphic?(fin, fin)
    assert :undecided == Agra.isomorphic?(fout, fout)
    assert false == Agra.isomorphic?(fin, fout)
  end

  test "agra wheel" do
    {h1in, h1out, h1inout, h2inout} = agrar(&AgraBuild.wheel/1)
    assert h1in == [1, 0, 9]
    assert h1out == [0, 9, 0, 0, 0, 0, 0, 0, 0, 1]
    assert h1inout == [0, 0, 0, 9, 0, 0, 0, 0, 0, 1]
    assert h2inout == [{{0, 9}, 1}, {{2, 1}, 9}]
  end

  test "agra clique" do
    {h1in, h1out, h1inout, h2inout} = agrar(&AgraBuild.clique/1)
    assert h1in == [0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    assert h1out == [0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    assert h1inout == [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
    assert h2inout == [{{9, 9}, 10}]
  end

  test "agra grid2d" do
    {h1in, h1out, h1inout, h2inout} = AgraBuild.grid2d(4, 3) |> render()
    assert h1in == [0, 0, 4, 6, 2]
    assert h1out == [0, 0, 4, 6, 2]
    assert h1inout == [0, 0, 0, 0, 4, 0, 6, 0, 2]
    assert h2inout == [{{2, 2}, 4}, {{3, 3}, 6}, {{4, 4}, 2}]
  end

  test "agra random" do
    g = AgraBuild.random(@n, 2 * @n)
    render(g)
    assert @n == Agra.nvert(g)
    assert 2 * @n == Agra.nedge(g)
  end

  # read/write AGR format

  test "to file" do
    path = AgraBuild.line(@n) |> Agra.to_agra_file(@out_dir)
    assert path == Path.join([@out_dir, "line_#{@n}.agr"])
  end

  test "from file" do
    line = "line_#{@n}" |> in_file() |> Agra.from_agra_file()
    assert Agra.equal?(line, AgraBuild.line(@n))
  end

  # -----------------
  # private utilities
  # -----------------

  defp agrar(fun_new), do: render(fun_new.(@n))

  defp render(g) do
    h1in = g |> Agra.degree_histo1d(:in) |> Histo1D.to_list()
    h1out = g |> Agra.degree_histo1d(:out) |> Histo1D.to_list()
    h1inout = g |> Agra.degree_histo1d(:inout) |> Histo1D.to_list()
    h2inout = g |> Agra.degree_histo2d() |> Histo2D.to_list()

    case Agra.to_agra_file(g, @out_dir) do
      {:error, msg} ->
        raise msg

      dotfile ->
        Enum.each([:png, :svg], fn fmt ->
          DotRender.render_dot(dotfile, fmt, @out_dir)
        end)

        {h1in, h1out, h1inout, h2inout}
    end
  end
end
