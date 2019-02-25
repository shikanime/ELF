use Mix.Config

config :elven_gard_citadel,
  ecto_repos: [ElvenGardCitadel.Datastore]

config :elven_gard_citadel, ElvenGardCitadel.Datastore,
  database: "elven_gard_citadel",
  username: "elven_gard_citadel",
  password: "postgres",
  hostname: "127.0.0.1",
  pool_size: 10
