defmodule Exa.Graf.DepReaderTest do
  use ExUnit.Case

  use Exa.Constants
  use Exa.Graf.Constants

  alias Exa.Graf.Gio
  alias Exa.Graf.Gio.DotRender

  import Exa.Graf.Gio.DepReader

  @in_dir Path.join(["test", "input", "graf", "dep"])
  @out_dir Path.join(["test", "output", "graf", "dep"])

  defp in_file(name), do: Path.join(@in_dir, name)

  test "simple" do
    for fname <- @in_dir |> File.ls() |> elem(1) do
      {g, gattrs} = fname |> in_file() |> from_dep_file()
      {dotfile, _dot} = Gio.to_dot_file(g, @out_dir, gattrs)

      Enum.each([:png, :svg], fn fmt ->
        DotRender.render_dot(dotfile, fmt, @out_dir)
      end)
    end
  end
end
