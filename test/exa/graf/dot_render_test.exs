defmodule Exa.Graf.DotRenderTest do
  use ExUnit.Case

  use Exa.Graf.Constants
  import Exa.Graf.DotRender

  @in_dir ["test", "input", "graf", "dot"]
  @in_files ["abcd", "squares", "test123", "small", "petersen"]

  @out_dir Path.join(["test", "output", "graf", "dot"])

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_dot)

  # render  ---------

  test "missing file" do
    {:error, _msg} = "xyz" |> in_file() |> render_dot()
  end

  test "dot input" do
    for file <- @in_files do
      path = in_file(file)
      png = render_dot(path, :png, @out_dir)
      assert String.ends_with?(png, file <> ".png")
      svg = render_dot(path, :svg, @out_dir)
      assert String.ends_with?(svg, file <> ".svg")
    end
  end
end
