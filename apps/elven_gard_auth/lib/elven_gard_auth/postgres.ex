defmodule ElvenGardAuth.Postgres do
  use Ecto.Repo,
    otp_app: :elven_gard_auth,
    adapter: Ecto.Adapters.Postgres
end
