defmodule ElvenGardGuard.Postgres do
  use Ecto.Repo,
    otp_app: :elven_gard_guard,
    adapter: Ecto.Adapters.Postgres

  def init(_type, config) do
    env_config = config
    |> put_env_config(:password, "POSTGRES_PASSWORD")
    |> put_env_config(:hostname, "POSTGRES_HOST")
    {:ok, env_config}
  end

  def put_env_config(config, key, name) do
    case System.get_env(name) do
      nil ->
        config

      value ->
        Keyword.put(config, key, value)
    end
  end
end
