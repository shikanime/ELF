use Mix.Config

config :elven_gard_universe,
  ecto_repos: [ElvenGardUniverse.Postgres]

  import_config "#{Mix.env()}.exs"
