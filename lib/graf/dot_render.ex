defmodule Exa.Graf.DotRender do
  @moduledoc """
  Render directed graph in GraphViz DOT format.

  DOT files are rendered to PNG or SVG, 
  iff [GraphViz](https://graphviz.org/docs/layouts/dot/) is installed.
  """
  require Logger

  use Exa.Graf.Constants

  alias Exa.Types, as: E

  alias Exa.Graf.DotTypes, as: D

  # the GraphViz DOT executable
  @exe :dot

  @doc """
  Render a DOT file to image or other output format.

  Assumes GraphViz is installed.

  If the input file path does not have a filetype, 
  then the default `.dot` is appended.

  If the optional output directory is not specified,
  the image file is written to the input directory.

  The return value is the full path to the output file, or error.
  """
  @spec render_dot(Path.t(), D.format(), nil | E.filename()) :: E.filename() | {:error, any()}
  def render_dot(in_path, format \\ :png, out_dir \\ nil) when is_atom(format) do
    in_path = to_string(in_path)
    fmt = format |> to_string() |> String.downcase()
    in_path = Exa.File.ensure_type(in_path, to_string(@filetype_dot))
    name = Path.basename(in_path) |> String.split(".", trim: true) |> hd()
    out_file = name <> "." <> fmt

    out_dir =
      if is_nil(out_dir) do
        Path.dirname(in_path)
      else
        Exa.File.ensure_dir!(out_dir)
      end

    out_path = Path.join([out_dir, out_file])

    opts = [stderr_to_stdout: true]

    case Exa.System.installed(@exe) do
      nil ->
        msg = "GraphViz 'dot' not installed"
        Logger.error(msg)
        {:error, msg}

      _exe ->
        Logger.info("Write #{String.upcase(fmt)} file: #{out_path}")

        case System.cmd(to_string(@exe), ["-T#{fmt}", in_path, "-o", out_path], opts) do
          {_, 0} -> out_path
          err -> {:error, err}
        end
    end
  rescue
    err -> {:error, err}
  end
end
