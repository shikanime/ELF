use Mix.Config

config :elven_gard_tower,
  ecto_repos: [ElvenGardTower.Datastore]

config :elven_gard_tower, ElvenGardTower.Datastore,
  database: "elven_gard_tower",
  username: "elven_gard_tower"
