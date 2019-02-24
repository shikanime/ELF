use Mix.Config

config :elven_gard_tower,
  ecto_repos: [ElvenGardTower.Postgres]

config :elven_gard_tower, ElvenGardTower.Postgres,
  database: "elven_gard_tower",
  username: "elven_gard_tower"
