defmodule ElvenGard.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_gard,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {ElvenGard.Application, []}
    ]
  end

  defp deps do
    [
      {:espec, "~> 1.6.3", only: :test},
      {:mock, "~> 0.3.0", only: :test},
      {:uuid, "~> 1.1"},
      {:ecto_sql, "~> 3.0"},
      {:postgrex, ">= 0.0.0"}
    ]
  end
end
