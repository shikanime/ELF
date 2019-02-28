defmodule ElvenGardBastion.Application do
  use Application

  def start(_type, _args) do
    topologies = [
      replicas: [
        strategy: Elixir.Cluster.Strategy.Gossip,
        config: [
          secret: Application.get_env(:libcluster, :bastion_secret, System.get_env("GOSSIP_SECRET"))
        ]
      ],
    ]

    children = [
      {Cluster.Supervisor, [topologies, [name: ElvenGardBastion.ClusterSupervisor]]},
      {ElvenGardBastion.Endpoint, []},
      {ElvenGardBastion.Swarm, []},
      {ElvenGardBastion.Postgres, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGardBastion.AppSupervisor]
    Supervisor.start_link(children, opts)
  end
end
