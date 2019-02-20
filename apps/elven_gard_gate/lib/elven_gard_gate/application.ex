defmodule ElvenGardGate.Application do
  use Application

  alias ElvenGardGate.LoginEndpoint
  alias ElvenGardGate.WorldEndpoint

  def start(_type, _args) do
    children = [
      {Cluster.Supervisor, [cluster_config(), [name: ElvenGardGate.ClusterSupervisor]]},
      {LoginEndpoint, []},
      {WorldEndpoint, []},
    ]

    opts = [strategy: :one_for_one, name: ElvenGardGate.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def cluster_config() do
    [
      gossip: [
        strategy: Elixir.Cluster.Strategy.Gossip,
        config: [
          secret: Application.get_env(:libcluster, :gossip_secret, System.get_env("GOSSIP_SECRET"))
        ]
      ]
    ]
  end
end
