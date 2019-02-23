defmodule ElvenGardTower.MixProject do
  use Mix.Project

  @base_dir File.cwd!()
  @version_file Path.join(@base_dir, "VERSION")

  def project do
    [
      app: :elven_gard_tower,
      version: version(),
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  defp version do
    case File.read(@version_file) do
      {:ok, binary} -> strip(binary)
      _ -> raise "could not read the version number from VERSION"
    end
  end

  defp deps do
    [
      {:argon2_elixir, "~> 2.0"},
      {:ecto_sql, "~> 3.0"},
      {:postgrex, ">= 0.0.0"},
      {:libcluster, "~> 3.0"}
    ]
  end

  defp aliases do
    [
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate", "test"]
    ]
  end

  defp strip(iodata) do
    :re.replace(iodata, "^[\s\r\n\t]+|[\s\r\n\t]+$", "", [:global, return: :binary])
  end
end
