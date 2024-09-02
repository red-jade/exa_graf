defmodule Exa.Graf.Constants do
  @moduledoc """
  Constants for graph utilities.
  """

  defmacro __using__(_) do
    quote do
      # graph file format for GraphViz DOT
      @filetype_dot :dot

      # graph file format for adj graph (Elixir term literal)
      @filetype_adj :adj

      # attribute key for the DOT node alias name
      @alias :alias
    end
  end
end
