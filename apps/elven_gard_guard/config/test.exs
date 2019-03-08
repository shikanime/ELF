use Mix.Config

config :elven_gard_guard, ElvenGardGuard.Postgres,
  username: "postgres",
  database: "elven_gard_guard_test",
  pool: Ecto.Adapters.SQL.Sandbox
