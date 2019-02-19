defmodule ElvenGardGate.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_gard_gate,
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
      mod: {ElvenGardGate.Application, []}
    ]
  end

  defp deps do
    [
      {:uuid, "~> 1.1"},
      {:elven_gard_auth, in_umbrella: true},
      {:libcluster, "~> 3.0"},
      {:swarm, "~> 3.0"},
      {:ranch, "~> 1.5"},
    ]
  end
end
