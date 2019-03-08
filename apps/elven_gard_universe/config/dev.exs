use Mix.Config

config :libcluster,
  gossip_secret: "no_secret"

config :elven_gard_universe, ElvenGardUniverse.Postgres,
  username: "postgres",
  password: "postgres",
  database: "elven_gard_universe_dev",
  hostname: "localhost",
  pool_size: 10
