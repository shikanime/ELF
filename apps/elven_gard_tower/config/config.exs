use Mix.Config

config :elven_gard_tower,
  ecto_repos: [ElvenGardTower.Datastore]

config :elven_gard_tower, ElvenGardTower.Datastore,
  database: "elven_gard_tower",
  username: "elven_gard_tower",
  password: "postgres",
  hostname: "127.0.0.1",
  pool_size: 10
