defmodule Exa.Graf.MixProject do
  use Mix.Project

  def project do
    [
      app: :exa_graf,
      name: "Exa Graf",
      version: "0.1.0",
      elixir: "~> 1.15",
      erlc_options: [:verbose, :report_errors, :report_warnings, :export_all],
      start_permanent: Mix.env() == :prod,
      deps: deps(:main) ++ deps(:support),
      docs: docs(),
      test_pattern: "*_test.exs",
      dialyzer: [flags: [:no_improper_lists]]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  def docs do
    [
      main: "readme",
      output: "doc/api",
      assets: %{"assets" => "assets"},
      extras: ["README.md"]
    ]
  end

  # runtime code dependencies ----------

  defp deps(:main) do
    [
      {:exa, path: "../exa_core"},
      {:exa_std, path: "../exa_std"},
      {:exa_space, path: "../exa_space"},
      {:exa_color, path: "../exa_color"}
    ]
  end

  defp deps(:tag) do
    [
      {:exa, git: "https://github.com/red-jade/exa_core.git", tag: "v0.1.6"},
      {:exa_std, git: "https://github.com/red-jade/exa_std.git", tag: "v0.1.7"},
      {:exa_space, git: "https://github.com/red-jade/exa_space.git", tag: "v0.1.7"},
      {:exa_color, git: "https://github.com/red-jade/exa_color.git", tag: "v0.1.7"}
    ]
  end

  # building, documenting, testing ----------

  defp deps(:support) do
    [
      # typechecking
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},

      # documentation
      {:ex_doc, "~> 0.30", only: [:dev, :test], runtime: false},

      # benchmarking
      {:benchee, "~> 1.0", only: [:dev, :test]}
    ]
  end
end
