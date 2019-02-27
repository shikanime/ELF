defmodule ElvenGardBastion.Application do
  use Application

  def start(_type, _args) do
    topologies = [
      bastion: [
        strategy: Elixir.Cluster.Strategy.Gossip,
        config: [
          secret: Application.get_env(:libcluster, :bastion_secret, System.get_env("GOSSIP_SECRET"))
        ]
      ],
      # citadel: [
      #   strategy: Elixir.Cluster.Strategy.Kubernetes.DNS,
      #   config: [
      #     service: "myapp-headless",
      #     application_name: "myapp",
      #     polling_interval: 10_000
      #   ]
      # ]
    ]

    children = [
      {Cluster.Supervisor, [topologies, [name: ElvenGardBastion.ClusterSupervisor]]},
      {ElvenGardBastion.Endpoint, []},
      {ElvenGardBastion.Swarm, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGardBastion.AppSupervisor]
    Supervisor.start_link(children, opts)
  end
end
