use Mix.Config

config :elven_gard_guard,
  ecto_repos: [ElvenGardGuard.Postgres]

import_config "#{Mix.env()}.exs"
