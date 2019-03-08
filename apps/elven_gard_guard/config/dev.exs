use Mix.Config

config :libcluster,
  gossip_secret: "no_secret"

config :elven_gard_guard, ElvenGardGuard.Postgres,
  username: "postgres",
  password: "postgres",
  database: "elven_gard_guard_dev",
  hostname: "localhost",
  pool_size: 10
