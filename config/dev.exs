use Mix.Config

config :logger, :console,
  format: "[$level] $message\n"

config :libcluster,
  debug: true
