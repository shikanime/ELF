defmodule ElvenGard.MixProject do
  use Mix.Project

  @base_dir File.cwd!()
  @version_file Path.join(@base_dir, "VERSION")

  def project do
    [
      app: :elven_gard,
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
      mod: {ElvenGard.Application, []}
    ]
  end

  defp deps do
    [
      {:elven_gard_tower, in_umbrella: true},
      {:espec, "~> 1.6.3", only: :test},
      {:mock, "~> 0.3.0", only: :test},
      {:uuid, "~> 1.1"},
      {:swarm, "~> 3.0"},
      {:ranch, "~> 1.5"},
      {:libcluster, "~> 3.0"}
    ]
  end
end
