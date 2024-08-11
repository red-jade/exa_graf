defmodule Exa.Graf.Constants do
  @moduledoc """
  Constants for graph utilities.
  """

  defmacro __using__(_) do
    quote do
      # graph file format for GraphViz DOT
      @filetype_dot :dot

      # graph file format for agra (Elixir term literal)
      @filetype_agr :agr

      # attribute key for the DOT node alias name
      @alias :alias
    end
  end
end
