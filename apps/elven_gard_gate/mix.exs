defmodule ElvenGardGate.MixProject do
  use Mix.Project

  def start(_type, _args) do
    DeferredConfig.populate(:elven_gard_gate)
  end

  def project do
    [
      app: :elven_gard_gate,
      version: "2.0.0-beta.1",
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
      {:deferred_config, "~> 0.1.0"},
      {:uuid, "~> 1.1"},
      {:elven_gard_tower, in_umbrella: true},
      {:swarm, "~> 3.0"},
      {:ranch, "~> 1.5"},
      {:libcluster, "~> 3.0"}
    ]
  end
end
