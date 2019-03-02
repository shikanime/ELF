defmodule ElvenGardGuard.Application do
  use Application

  def start(_type, _args) do
    topologies = [
      replicas: [
        strategy: Elixir.Cluster.Strategy.Gossip,
        config: [
          secret: Application.get_env(:libcluster, :gossip_secret, System.get_env("GOSSIP_SECRET"))
        ]
      ],
    ]

    children = [
      {Cluster.Supervisor, [topologies, [name: ElvenGardGuard.Cluster.Supervisor]]},
      {ElvenGardGuard.Postgres, []},
      {ElvenGardGuard.Swarm, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGardGuard.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
