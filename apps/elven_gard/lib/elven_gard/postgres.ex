defmodule ElvenGard.Postgres do
  use Ecto.Repo,
    otp_app: :elven_gard,
    adapter: Ecto.Adapters.Postgres

  def init(_type, config) do
    {:ok, [
      password: System.get_env("POSTGRES_PASSWORD"),
      hostname: System.get_env("POSTGRES_HOST")
    ] ++ config}
  end
end
