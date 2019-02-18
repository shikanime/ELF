defmodule ElvenGardAuth.Repo do
  use Ecto.Repo,
    otp_app: :elvenGard_auth,
    adapter: Ecto.Adapters.Postgres
end
