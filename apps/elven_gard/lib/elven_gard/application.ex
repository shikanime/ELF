defmodule ElvenGard.Application do
  use Application

  def start(_type, _args) do
    children = [
      {ElvenGard.Cluster, []},
      {ElvenGard.Endpoint, []},
      {ElvenGardTower.Supervisor, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGard.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
