defmodule ElvenGardGuard.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_gard_guard,
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
      mod: {ElvenGardGuard.Application, []}
    ]
  end

  defp deps do
    [
      {:argon2_elixir, "~> 2.0"},
      {:ecto_sql, "~> 3.0"},
      {:postgrex, ">= 0.0.0"},
      {:espec, "~> 1.6.3", only: :test},
      {:mock, "~> 0.3.0", only: :test},
      {:uuid, "~> 1.1"},
      {:swarm, "~> 3.0"},
      {:libcluster, "~> 3.0"}
    ]
  end
end
