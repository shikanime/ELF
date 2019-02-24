defmodule ElvenGardTower.Datastore do
  use Ecto.Repo,
    otp_app: :elven_gard_tower,
    adapter: Ecto.Adapters.Postgres

  def init(_type, config) do
    {:ok, [
      password: System.get_env("POSTGRES_PWD"),
      hostname: System.get_env("POSTGRES_HOST")
    ] ++ config}
  end
end
