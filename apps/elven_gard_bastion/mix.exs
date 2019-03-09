defmodule ElvenGardBastion.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_gard_bastion,
      version: "2.0.0-beta.3",
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
      mod: {ElvenGardBastion.Application, []}
    ]
  end

  defp deps do
    [
      {:elven_gard_lib, in_umbrella: true},
      {:elven_gard_guard, in_umbrella: true}, # TODO: Dicouple services
      {:espec, "~> 1.6.3", only: :test},
      {:mock, "~> 0.3.0", only: :test},
      {:libcluster, "~> 3.0"},
      {:gen_state_machine, "~> 2.0"},
      {:ranch, "~> 1.5"}
    ]
  end
end
