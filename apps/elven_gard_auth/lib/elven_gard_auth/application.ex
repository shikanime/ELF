defmodule ElvenGardAuth.Application do
  use Application

  alias ElvenGardAuth.LoginEndpoint
  alias ElvenGardAuth.WorldEndpoint

  def start(_type, _args) do
    children = [
      {LoginEndpoint, []},
      {WorldEndpoint, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGardAuth.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
