defmodule ElvenGardUniverse.Application do
  @moduledoc false

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
      {Cluster.Supervisor, [topologies, [name: ElvenGardUniverse.Cluster.Supervisor]]},
      {ElvenGardUniverse.Postgres, []},
      {ElvenGardUniverse.Swarm, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGardUniverse.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
