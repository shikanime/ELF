defmodule ElvenGard.Application do
  use Application

  def start(_type, _args) do
    children = [
      {ElvenGard.Cluster, []},
      {ElvenGard.Endpoint, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGard.AppSupervisor]
    Supervisor.start_link(children, opts)
  end
end
