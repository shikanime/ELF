defmodule ElvenGardGate.Application do
  use Application

  alias ElvenGardGate.LoginEndpoint
  alias ElvenGardGate.WorldEndpoint

  def start(_type, _args) do
    children = [
      {LoginEndpoint, []},
      {WorldEndpoint, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGardGate.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
