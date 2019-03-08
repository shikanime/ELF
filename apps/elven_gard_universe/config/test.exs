use Mix.Config

config :elven_gard_universe, ElvenGardUniverse.Postgres,
  username: "postgres",
  database: "elven_gard_universe_test",
  pool: Ecto.Adapters.SQL.Sandbox
