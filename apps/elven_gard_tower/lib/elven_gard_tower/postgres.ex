defmodule ElvenGardTower.Postgres do
  use Ecto.Repo,
    otp_app: :elven_gard_tower,
    adapter: Ecto.Adapters.Postgres
end
