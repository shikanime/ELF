defmodule ElvenGardBastion.Application do
  use Application

  def start(_type, _args) do
    children = [
      {ElvenGardBastion.Cluster, []},
      {ElvenGardBastion.Endpoint, []},
      {ElvenGardBastion.Swarm, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGardBastion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
